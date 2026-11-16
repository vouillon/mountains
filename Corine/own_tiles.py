#!/usr/bin/env python3
"""Our own elevation tiles, from IGN's LIDAR HD (src/web/hd_dem.ml, [Own_tiles]).

    own_tiles.py <state dir> <region> [--out DIR | --bucket B --endpoint URL]
                 [--prefix v1] [--jobs N] [bx by ...]

<region> is lamb93 (metropolitan France, EPSG:2154) or rgr92utm40s (La Reunion,
EPSG:2975), the CRS each territory's layers are stored in, so the service never
reprojects them. Tiles go to <out>/<prefix>/<region>/... on disk, or with
--bucket to an S3-compatible bucket (Cloudflare R2: --endpoint
https://<account>.r2.cloudflarestorage.com, credentials in AWS_ACCESS_KEY_ID and
AWS_SECRET_ACCESS_KEY), each block deleted locally once uploaded. The state
dir keeps the block list and done.jsonl, the blocks finished: a run picks up
where the last one stopped. With block coordinates (bx by, in units of 4096 m),
only those blocks are built.

Needs numpy, GDAL's Python bindings (osgeo.osr, for the area) and, to upload,
boto3.

The area is where the client lets the eye go (Viewer.in_range: the dataset
boxes shrunk by the base grid's half extent) plus 10 km, so that every ring of
every allowed position is whole; within it, the blocks the service has data
for, from one coarse GetMap of the whole area, plus their neighbours.

Blocks are 4096 m squares on a 4096 m grid (one 16 m tile). Ground: 1 m from
the WMS, then 2, 4 and 8 m as exact box means (the service's own 2 m ground is
shifted by 1 m). Canopy: 2 m from the WMS (itself the exact 2 x 2 mean of its
1 m), then 4, 8 and 16 m. A coarse sample is nodata if any of its children is.
Only the levels the client's rings read are written: ground 1, 4, 8 m and
canopy 2, 4, 16 m.

Tiles: 256 x 256 samples, row 0 northernmost, pixel-is-area: tile (c, r) of
spacing s covers x in [c 256 s, (c + 1) 256 s), y in [r 256 s, (r + 1) 256 s).
File <prefix>/<region>/<product>/<s>/<c>_<r>.hdt, absent where the tile is all
nodata -- which the client caches as such, so a tile set that grows must move to
a new prefix.

HDT1 (the .dem layout of compress_dem.ml with a per-tile scale):
  0 "HDT1"  4 width u32  8 height u32  12 base f32  16 step f32
  20 high_size u32  24 low_size u32  28 zlib(high bytes)  zlib(low bytes)
height = base + step * u, u = 65535 is nodata; u coded as the residual of the
parallelogram predictor (0 outside the tile), zigzagged, split into bytes.

Ground is quantised to 1 cm (coarser per tile only if its range needs it).
Canopy is stored as the canopy bake's own steps (Viewer.canopy_texture,
canopy_prep.frag: floor (max (v, 0) * 5 + 0.5), in float32 as highp): base 0,
step 0.2, so that the bake of the decoded tiles is the bake of IGN's raw
heights sample for sample. Rounded to 10 cm first, 16% of the baked samples
moved by a step and the tree finder's local maxima with them (+47 broadleaves
of 2536 at 44.3811, 6.6311); as it is, the trees are IGN's exactly.
"""
import argparse, json, os, shutil, struct, sys, threading, time, urllib.error, urllib.request, zlib
from concurrent.futures import ThreadPoolExecutor
import numpy as np

B = 4096  # block side, m
T = 256  # tile side, samples
MNT = "IGNF_LIDAR-HD_MNT_ELEVATION.ELEVATIONGRIDCOVERAGE"
MNH = "IGNF_LIDAR-HD_MNH_ELEVATION.ELEVATIONGRIDCOVERAGE"
WRITE = {"ground": (1, 4, 8), "canopy": (2, 4, 16)}
MARGIN_M = 10_000

# Viewer.in_range's boxes and the half extent it requires (0.57 degrees at
# size 4096), with each territory's layer suffix and EPSG code.
REGIONS = {
    "lamb93": dict(suffix="LAMB93", epsg=2154, box=(42, 47, 4, 10)),
    "rgr92utm40s": dict(suffix="RGR92UTM40S", epsg=2975, box=(-22, -20, 54, 57)),
}
HALF_EXTENT_DEG = 2048 / 3600


def wms(layer, epsg, x, y, w_m, h_m, w_px, h_px):
    """GetMap as float32 heights, rows from the north; nodata as NaN."""
    url = (
        "https://data.geopf.fr/wms-r/wms?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap"
        "&LAYERS=%s&STYLES=&CRS=EPSG:%d&FORMAT=image/x-bil;bits=32"
        "&BBOX=%.3f,%.3f,%.3f,%.3f&WIDTH=%d&HEIGHT=%d"
        % (layer, epsg, x, y, x + w_m, y + h_m, w_px, h_px)
    )
    for attempt in range(8):
        try:
            d = urllib.request.urlopen(url, timeout=300).read()
            if len(d) == w_px * h_px * 4:
                a = np.frombuffer(d, "<f4").reshape(h_px, w_px).copy()
                a[a < -1000] = np.nan
                return a
            err = "short body %d" % len(d)
        except urllib.error.HTTPError as e:
            err = "HTTP %d" % e.code
        except Exception as e:
            err = repr(e)
        print("  retry", attempt, err, x, y, flush=True)
        time.sleep(3 * (attempt + 1))
    raise RuntimeError("gave up on %s" % url)


def halve(a):
    n = a.shape[0] // 2
    return a.reshape(n, 2, n, 2).mean(axis=(1, 3))  # NaN if any child is


def pack(q, base, step):
    """HDT1 of a T x T array of u16 steps (65535 nodata)."""
    p = np.zeros((T + 1, T + 1), np.int64)
    p[1:, 1:] = q
    r = (p[1:, 1:] - p[1:, :-1] - p[:-1, 1:] + p[:-1, :-1]) & 0xFFFF
    s = np.where(r >= 0x8000, r - 0x10000, r)
    z = ((s << 1) ^ (s >> 15)) & 0xFFFF
    hb = zlib.compress((z >> 8).astype(np.uint8).tobytes(), 9)
    lb = zlib.compress((z & 0xFF).astype(np.uint8).tobytes(), 9)
    blob = b"HDT1" + struct.pack("<IIffII", T, T, base, step, len(hb), len(lb)) + hb + lb
    # Decoded as the client does (a 2-D prefix sum mod 2^16), it must give q back.
    n1, n2 = struct.unpack("<II", blob[20:28])
    zz = (np.frombuffer(zlib.decompress(blob[28:28 + n1]), np.uint8).astype(np.int64) << 8) | np.frombuffer(
        zlib.decompress(blob[28 + n1:28 + n1 + n2]), np.uint8
    )
    rr = (zz >> 1) ^ -(zz & 1)
    assert ((np.cumsum(np.cumsum(rr.reshape(T, T), 0), 1) & 0xFFFF) == q).all()
    return blob


def encode_ground(h, step=0.01):
    h = h.astype(np.float64)
    ok = ~np.isnan(h)
    if not ok.any():
        return None
    lo, hi = float(h[ok].min()), float(h[ok].max())
    step = max(step, np.ceil((hi - lo) / 65533 * 1000) / 1000)
    step32 = float(np.float32(step))
    base32 = float(np.float32(np.floor(lo / step32) * step32))
    q = np.full(h.shape, 65535, np.int64)
    q[ok] = np.rint((h[ok] - base32) / step32).astype(np.int64)
    assert q[ok].min() >= 0 and q[ok].max() <= 65534
    assert np.abs(base32 + step32 * q[ok] - h[ok]).max() <= step32 / 2 + 1e-3
    return pack(q, base32, step32)


def bake_steps(h):
    v = np.maximum(h.astype(np.float32), np.float32(0))
    return np.floor((v * np.float32(5)) + np.float32(0.5)).astype(np.int64)


def encode_canopy(h):
    ok = ~np.isnan(h)
    if not ok.any():
        return None
    q = np.full(h.shape, 65535, np.int64)
    q[ok] = bake_steps(h[ok])
    assert q[ok].max() <= 65534
    # The client decodes base + step * u into float32: that must bake back to q.
    back = (np.float32(0.0) + np.float32(0.2) * q[ok].astype(np.float64)).astype(np.float32)
    assert (bake_steps(back) == q[ok]).all()
    return pack(q, 0.0, 0.2)


def write_level(root, product, s, a, bx, by, files):
    n = a.shape[0] // T
    d = os.path.join(root, product, "%g" % s)
    os.makedirs(d, exist_ok=True)
    encode = encode_canopy if product == "canopy" else encode_ground
    for tr in range(n):
        for tc in range(n):
            # raster row 0 is north: tile row tr from the top is grid row n - 1 - tr
            c = bx * B // int(T * s) + tc
            r = by * B // int(T * s) + (n - 1 - tr)
            blob = encode(a[tr * T:(tr + 1) * T, tc * T:(tc + 1) * T])
            if blob is None:
                continue
            name = os.path.join(d, "%d_%d.hdt" % (c, r))
            with open(name, "wb") as f:
                f.write(blob)
            files.append(name)


def build_block(region, bx, by, root, pool):
    """Fetch block (bx, by) and write its tiles under root; their paths."""
    sfx, epsg = REGIONS[region]["suffix"], REGIONS[region]["epsg"]
    x, y = bx * B, by * B
    # Ground: 2 x 2 GetMaps of 2048^2 at 1 m, piece (qr, qc), qr 0 the northern band.
    futs = {
        (qr, qc): pool.submit(wms, MNT + "." + sfx, epsg, x + qc * 2048, y + (1 - qr) * 2048, 2048, 2048, 2048, 2048)
        for qr in (0, 1)
        for qc in (0, 1)
    }
    fc = pool.submit(wms, MNH + "." + sfx, epsg, x, y, B, B, 2048, 2048)
    g = np.empty((B, B), np.float32)
    for (qr, qc), f in futs.items():
        g[qr * 2048:(qr + 1) * 2048, qc * 2048:(qc + 1) * 2048] = f.result()
    c = fc.result()
    files = []
    for s in (1, 2, 4, 8):
        if s in WRITE["ground"]:
            write_level(root, "ground", s, g, bx, by, files)
        if s < 8:
            g = halve(g)
    for s in (2, 4, 8, 16):
        if s in WRITE["canopy"]:
            write_level(root, "canopy", s, c, bx, by, files)
        if s < 16:
            c = halve(c)
    return files


def plan_blocks(region, state):
    """The blocks to build, computed once and kept in the state dir."""
    path = os.path.join(state, "blocks.json")
    if os.path.exists(path):
        return [tuple(b) for b in json.load(open(path))]
    from osgeo import osr

    osr.UseExceptions()

    info = REGIONS[region]
    lat0, lat1, lon0, lon1 = info["box"]
    lat0, lat1 = lat0 + HALF_EXTENT_DEG, lat1 - HALF_EXTENT_DEG
    lon0, lon1 = lon0 + HALF_EXTENT_DEG, lon1 - HALF_EXTENT_DEG
    wgs, prj = osr.SpatialReference(), osr.SpatialReference()
    wgs.ImportFromEPSG(4326)
    prj.ImportFromEPSG(info["epsg"])
    for s in (wgs, prj):
        s.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)  # lon, lat / x, y
    fwd, inv = osr.CoordinateTransformation(wgs, prj), osr.CoordinateTransformation(prj, wgs)
    # The box's outline in the CRS, widened by the margin.
    edge = [(lon0 + (lon1 - lon0) * i / 50, lat) for i in range(51) for lat in (lat0, lat1)] + [
        (lon, lat0 + (lat1 - lat0) * i / 50) for i in range(51) for lon in (lon0, lon1)
    ]
    pts = [fwd.TransformPoint(lon, lat)[:2] for lon, lat in edge]
    bx0 = int((min(p[0] for p in pts) - MARGIN_M) // B)
    bx1 = int((max(p[0] for p in pts) + MARGIN_M) // B)
    by0 = int((min(p[1] for p in pts) - MARGIN_M) // B)
    by1 = int((max(p[1] for p in pts) + MARGIN_M) // B)
    # Where the service has data: one GetMap of the area at 16 samples per block.
    nx, ny = bx1 - bx0 + 1, by1 - by0 + 1
    assert nx * 16 <= 5000 and ny * 16 <= 5000, "area too large for one coverage GetMap"
    cov = wms(MNT + "." + info["suffix"], info["epsg"], bx0 * B, by0 * B, nx * B, ny * B, nx * 16, ny * 16)
    has = ~np.isnan(cov).reshape(ny, 16, nx, 16).all(axis=(1, 3))[::-1]  # [j][i], j from the south
    # Neighbours too: data that a 256 m sampling missed.
    grown = has.copy()
    grown[1:] |= has[:-1]
    grown[:-1] |= has[1:]
    grown[:, 1:] |= grown[:, :-1].copy()
    grown[:, :-1] |= grown[:, 1:].copy()
    # Within the margin of the box: a block's centre within it, widened by half a
    # block's diagonal (in degrees, generously: 1 degree >= 78 km of longitude here).
    pad_lat = (MARGIN_M + 2900) / 111_000
    blocks = []
    for j in range(ny):
        for i in range(nx):
            if not grown[j, i]:
                continue
            bx, by = bx0 + i, by0 + j
            lon, lat = inv.TransformPoint((bx + 0.5) * B, (by + 0.5) * B)[:2]
            pad_lon = pad_lat / np.cos(np.radians(lat))
            if lat0 - pad_lat <= lat <= lat1 + pad_lat and lon0 - pad_lon <= lon <= lon1 + pad_lon:
                blocks.append((bx, by))
    json.dump(blocks, open(path, "w"))
    print("%s: %d blocks of %d in the area's bounding box have data" % (region, len(blocks), nx * ny), flush=True)
    return blocks


class Uploader:
    def __init__(self, bucket, endpoint, workers=16):
        import boto3

        self.s3 = boto3.client("s3", endpoint_url=endpoint)
        self.bucket = bucket
        self.pool = ThreadPoolExecutor(workers)

    def put(self, path, key):
        with open(path, "rb") as f:
            self.s3.put_object(
                Bucket=self.bucket,
                Key=key,
                Body=f.read(),
                ContentType="application/octet-stream",
                CacheControl="public, max-age=31536000, immutable",
            )

    def upload(self, files, staging, prefix):
        keys = [prefix + "/" + os.path.relpath(p, staging).replace(os.sep, "/") for p in files]
        list(self.pool.map(self.put, files, keys))


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("state")
    ap.add_argument("region", choices=sorted(REGIONS))
    ap.add_argument("blocks", nargs="*", type=int)
    ap.add_argument("--out", help="write the tiles under this directory")
    ap.add_argument("--bucket", help="upload the tiles to this bucket")
    ap.add_argument("--endpoint", help="the bucket's S3 endpoint")
    ap.add_argument("--prefix", default="v1")
    ap.add_argument("--jobs", type=int, default=2, help="blocks at a time (5 GetMaps each)")
    args = ap.parse_args()
    if bool(args.out) == bool(args.bucket):
        ap.error("give exactly one of --out and --bucket")
    os.makedirs(args.state, exist_ok=True)
    todo = list(zip(args.blocks[::2], args.blocks[1::2])) or plan_blocks(args.region, args.state)
    done_path = os.path.join(args.state, "done.jsonl")
    done = set()
    if os.path.exists(done_path):
        for line in open(done_path):
            j = json.loads(line)
            done.add((j["bx"], j["by"]))
    uploader = Uploader(args.bucket, args.endpoint) if args.bucket else None
    fetch_pool = ThreadPoolExecutor(5 * args.jobs)
    lock = threading.Lock()
    todo = [b for b in todo if b not in done]
    print("%d blocks to build" % len(todo), flush=True)
    counter = [0]

    def run(b):
        bx, by = b
        t0 = time.time()
        if uploader:
            staging = os.path.join(args.state, "staging", "%d_%d" % b)
            root = os.path.join(staging, args.region)
            files = build_block(args.region, bx, by, root, fetch_pool)
            uploader.upload(files, staging, args.prefix)
            shutil.rmtree(staging)
        else:
            root = os.path.join(args.out, args.prefix, args.region)
            files = build_block(args.region, bx, by, root, fetch_pool)
        size = sum(os.path.getsize(f) for f in files) if not uploader else None
        with lock:
            with open(done_path, "a") as f:
                f.write(json.dumps({"bx": bx, "by": by, "files": len(files), "s": round(time.time() - t0, 1)}) + "\n")
            counter[0] += 1
            print("block %d %d: %d files%s, %.0f s (%d/%d)"
                  % (bx, by, len(files), "" if size is None else ", %.1f MB" % (size / 1e6), time.time() - t0,
                     counter[0], len(todo)), flush=True)

    with ThreadPoolExecutor(args.jobs) as blocks:
        list(blocks.map(run, todo))
    print("ALL DONE", flush=True)


if __name__ == "__main__":
    sys.exit(main())
