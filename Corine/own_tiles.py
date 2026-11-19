#!/usr/bin/env python3
"""Our own elevation tiles, from IGN's LIDAR HD (src/web/hd_dem.ml, [Own_tiles]).

    own_tiles.py <state dir> <region> [--out DIR | --bucket B --endpoint URL]
                 [--prefix v1] [--jobs N] [--swiss | --italy] [bx by ...]

<region> is lamb93 (metropolitan France, EPSG:2154) or rgr92utm40s (La Reunion,
EPSG:2975), the CRS each territory's layers are stored in, so the service never
reprojects them. Tiles go to <out>/<prefix>/<region>/... on disk, or with
--bucket to an S3-compatible bucket (Cloudflare R2: --endpoint
https://<account>.r2.cloudflarestorage.com, credentials in AWS_ACCESS_KEY_ID and
AWS_SECRET_ACCESS_KEY), each block deleted locally once uploaded. The state
dir keeps the block list and done.jsonl, the blocks finished: a run picks up
where the last one stopped. With block coordinates (bx by, in units of 4096 m),
only those blocks are built.

Needs numpy, GDAL's Python bindings (osgeo, for the area and swisstopo's tiles)
and, to upload, boto3.

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

Switzerland (--swiss, lamb93 only, with a state dir of its own): the blocks
swisstopo's swissALTI3D (ground) and swissSURFACE3D Raster (surface) touch within
the area, rebuilt from both countries and written over the same tile names (a
browser that read the old ones, or their absence, keeps them until its site data
is cleared, and Cloudflare's cache until it is purged). Their
0.5 m tiles are read at 1 m from the files' first overview, which is every other
sample (not a mean) and so lies a quarter metre north-west of where the file puts
it: shifted back, it is within 2.3 cm RMS of the mean of the 0.5 m samples, for a
quarter of the download. Canopy is surface minus the ground of the surface's
year (see swiss_index: not the latest, over glaciers). Swiss heights (LN02) are
0.25-0.5 m below IGN's (NGF-IGN69) along the border, and the two datasets overlap
by hundreds of metres there: where only one has data, it is used; across the
overlap, IGN fades to swisstopo over the 200 m next to the Swiss-only ground, so
the difference becomes a slope of at most 0.25% rather than a step. Blocks are
fetched with 256 m around them, so that the fade sees across their edges.

Italy (--italy, lamb93 only, a state dir of its own, after --swiss): the blocks
TINITALY covers within the area, plus their neighbours, from IGN, swisstopo and
two Italian models: Regione Piemonte's 5 m LIDAR DTM (ICE 2009-2011, its WCS),
and where it has none, INGV's TINITALY 1.1 at 10 m (both CC BY 4.0). Piemonte
is within 0.2 m of IGN on flat ground at the border (Col de Larche, Col de
Tende); TINITALY within 0.6 m but noisy, and 12 m RMS off on the glaciers by the
Mont Blanc tunnel, older than IGN's: IGN and swisstopo fade to them over 500 m,
Piemonte to TINITALY over 200 m. Neither resolves a 1 m sample, so the 1 m
ground stays where IGN or swisstopo has data, taken from the blend (so that it
meets the 4 m ring where it ends) and the canopy is IGN's and swisstopo's alone.

The runs that built tiles.vouillon.org/v1, each with its state dir under
~/mountains-tiles (where the TINITALY download also stays), in this order: a
later one rewrites the blocks it shares with an earlier one.

    own_tiles.py state-lamb93 lamb93 --bucket B --endpoint E          IGN
    own_tiles.py state-reunion rgr92utm40s --bucket B --endpoint E    IGN
    own_tiles.py state-swiss lamb93 --swiss --bucket B --endpoint E   + swisstopo
    own_tiles.py state-lamb93-edges lamb93 ...      the IGN blocks with an edge,
    own_tiles.py state-reunion-edges rgr92utm40s ...  from own_tiles_edges.py,
    own_tiles.py state-swiss-edges lamb93 --swiss ...  once wms dropped IGN's
                                                       mixtures at its nodata
    own_tiles.py state-italy lamb93 --italy --bucket B --endpoint E   + Italy
    own_tiles_stale.py lamb93=state-... --bucket B ...      the tiles a rebuild
                                                       emptied (no run deletes)

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
import argparse, json, os, re, shutil, struct, sys, threading, time, urllib.error, urllib.request, zlib
from concurrent.futures import ThreadPoolExecutor
import numpy as np

B = 4096  # block side, m
T = 256  # tile side, samples
MNT = "IGNF_LIDAR-HD_MNT_ELEVATION.ELEVATIONGRIDCOVERAGE"
MNH = "IGNF_LIDAR-HD_MNH_ELEVATION.ELEVATIONGRIDCOVERAGE"
WRITE = {"ground": (1, 4, 8), "canopy": (2, 4, 16)}
MARGIN_M = 10_000
SWISS = {"ground": "ch.swisstopo.swissalti3d", "surface": "ch.swisstopo.swisssurface3d-raster"}
PAD = 256  # m fetched around a Swiss block
FEATHER = 200  # m over which IGN fades to swisstopo
ITALY_PAD = 512  # m fetched around an Italian block
ITALY_FEATHER = 500  # m over which IGN and swisstopo fade to the Italian models
TINITALY = "https://tinitaly.pi.ingv.it/"
PIEMONTE = "https://geomap.reteunitaria.piemonte.it/ws/taims/rp-01/taimsdtmwcs/wcs_ice_2009_2011_dtm"
# Its coverage's bounding box in UTM 32N, as the service states it: a request
# outside it gets an exception report, not an empty image.
PIEMONTE_BOX = (290748, 4861375, 536649, 5165171)

# Viewer.in_range's boxes and the half extent it requires (0.57 degrees at
# size 4096), with each territory's layer suffix and EPSG code.
REGIONS = {
    "lamb93": dict(suffix="LAMB93", epsg=2154, box=(42, 47, 4, 10)),
    "rgr92utm40s": dict(suffix="RGR92UTM40S", epsg=2975, box=(-22, -20, 54, 57)),
}
HALF_EXTENT_DEG = 2048 / 3600


def wms(layer, epsg, x, y, w_m, h_m, w_px, h_px, clean=True):
    """GetMap as float32 heights, rows from the north; nodata as NaN.

    The service resamples its data with its nodata, -9999, as a height, so the
    samples next to it are mixtures: 1948, 766 and -16 m in a row on a 2540 m
    slope at the Italian border (Argentera), trenches in every ring along every
    edge of the data. They go out to one sample, diagonals included, the same
    whatever the request: with clean, the request is two samples wider on each
    side, and every sample within two of nodata is dropped."""
    if clean:
        sx, sy = w_m / w_px, h_m / h_px
        a = wms(layer, epsg, x - 2 * sx, y - 2 * sy, w_m + 4 * sx, h_m + 4 * sy, w_px + 4, h_px + 4, clean=False)
        bad = np.isnan(a)
        for _ in range(2):
            p = np.pad(bad, 1, mode="edge")
            bad = (p[1:-1, 1:-1] | p[:-2, 1:-1] | p[2:, 1:-1] | p[1:-1, :-2] | p[1:-1, 2:]
                   | p[:-2, :-2] | p[:-2, 2:] | p[2:, :-2] | p[2:, 2:])
        a[bad] = np.nan
        return a[2:-2, 2:-2].copy()
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


def area(region):
    """The area's bounding box in blocks, its test of a block, its lat/lon box
    and the projections to and from WGS 84 (lon, lat)."""
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
    # Within the margin of the box: a block's centre within it, widened by half a
    # block's diagonal (in degrees, generously: 1 degree >= 78 km of longitude here).
    pad_lat = (MARGIN_M + 2900) / 111_000

    def inside(bx, by):
        lon, lat = inv.TransformPoint((bx + 0.5) * B, (by + 0.5) * B)[:2]
        pad_lon = pad_lat / np.cos(np.radians(lat))
        return lat0 - pad_lat <= lat <= lat1 + pad_lat and lon0 - pad_lon <= lon <= lon1 + pad_lon

    box = (lat0 - pad_lat, lat1 + pad_lat, lon0 - 2 * pad_lat, lon1 + 2 * pad_lat)
    return (bx0, bx1, by0, by1), inside, box, fwd, inv


def plan_blocks(region, state):
    """The blocks to build, computed once and kept in the state dir."""
    path = os.path.join(state, "blocks.json")
    if os.path.exists(path):
        return [tuple(b) for b in json.load(open(path))]
    info = REGIONS[region]
    (bx0, bx1, by0, by1), inside, _, _, _ = area(region)
    # Where the service has data: one GetMap of the area at 16 samples per block.
    nx, ny = bx1 - bx0 + 1, by1 - by0 + 1
    assert nx * 16 <= 5000 and ny * 16 <= 5000, "area too large for one coverage GetMap"
    cov = wms(MNT + "." + info["suffix"], info["epsg"], bx0 * B, by0 * B, nx * B, ny * B, nx * 16, ny * 16, clean=False)
    has = ~np.isnan(cov).reshape(ny, 16, nx, 16).all(axis=(1, 3))[::-1]  # [j][i], j from the south
    # Neighbours too: data that a 256 m sampling missed.
    grown = has.copy()
    grown[1:] |= has[:-1]
    grown[:-1] |= has[1:]
    grown[:, 1:] |= grown[:, :-1].copy()
    grown[:, :-1] |= grown[:, 1:].copy()
    blocks = [(bx0 + i, by0 + j) for j in range(ny) for i in range(nx) if grown[j, i] and inside(bx0 + i, by0 + j)]
    json.dump(blocks, open(path, "w"))
    print("%s: %d blocks of %d in the area's bounding box have data" % (region, len(blocks), nx * ny), flush=True)
    return blocks


def get_json(url):
    for attempt in range(8):
        try:
            return json.load(urllib.request.urlopen(url, timeout=120))
        except Exception as e:
            print("  retry", attempt, repr(e), url, flush=True)
            time.sleep(3 * (attempt + 1))
    raise RuntimeError("gave up on %s" % url)


def swiss_index(state, box):
    """The 0.5 m GeoTIFFs swisstopo has of each 1 km tile, by tile name ("<east
    km>-<north km>", its south-west corner in LV95), from its STAC API: {"ground":
    latest swissALTI3D, "surface": latest swissSURFACE3D, "under": the
    swissALTI3D the surface is measured from}, each a URL; kept in the state dir.

    "under" is the ground of the surface's year or, failing it, of the latest
    year before (else the earliest after): the latest ground can be younger than
    the surface, and a glacier that thinned in between then rises above it as
    metres of canopy (2615-1091: ground 2024, surface 2022). From an older
    ground it sinks below, which the canopy encoding clamps to 0."""
    path = os.path.join(state, "swiss_tiles.json")
    if os.path.exists(path):
        return json.load(open(path))
    lat0, lat1, lon0, lon1 = box
    index = {}
    for kind, coll in SWISS.items():
        url = "https://data.geo.admin.ch/api/stac/v0.9/collections/%s/items?bbox=%f,%f,%f,%f&limit=100" % (
            coll, lon0, lat0, lon1, lat1)
        years = {}
        while url:
            j = get_json(url)
            for f in j["features"]:
                year, name = f["id"].rsplit("_", 2)[-2:]
                hrefs = [a["href"] for a in f["assets"].values() if a["href"].endswith("_0.5_2056_5728.tif")]
                if hrefs:
                    years.setdefault(name, {})[int(year)] = hrefs[0]
            url = next((l["href"] for l in j["links"] if l["rel"] == "next"), None)
        index[kind] = years
        print("%s: %d tiles" % (coll, len(years)), flush=True)
    ground, surface = index["ground"], index["surface"]
    under = {}
    for name, ys in surface.items():
        gs = ground.get(name)
        if gs:
            y = max(ys)
            before = [g for g in gs if g <= y]
            under[name] = gs[max(before) if before else min(gs)]
    index = {"ground": {n: ys[max(ys)] for n, ys in ground.items()},
             "surface": {n: ys[max(ys)] for n, ys in surface.items()}, "under": under}
    json.dump(index, open(path, "w"))
    return index


def lv95():
    from osgeo import osr

    osr.UseExceptions()
    ch, l93 = osr.SpatialReference(), osr.SpatialReference()
    ch.ImportFromEPSG(2056)
    l93.ImportFromEPSG(2154)
    for s in (ch, l93):
        s.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    return osr.CoordinateTransformation(ch, l93), osr.CoordinateTransformation(l93, ch)


def plan_swiss(state):
    """The lamb93 blocks within the area that a swisstopo ground tile touches."""
    path = os.path.join(state, "blocks.json")
    if os.path.exists(path):
        return [tuple(b) for b in json.load(open(path))]
    _, inside, box, _, _ = area("lamb93")
    index = swiss_index(state, box)
    to_l93, _ = lv95()
    blocks = set()
    for name in index["ground"]:
        e, n = (int(v) * 1000 for v in name.split("-"))
        pts = [to_l93.TransformPoint(e + dx, n + dy)[:2] for dx in (0, 1000) for dy in (0, 1000)]
        for bx in range(int(min(p[0] for p in pts) // B), int(max(p[0] for p in pts) // B) + 1):
            for by in range(int(min(p[1] for p in pts) // B), int(max(p[1] for p in pts) // B) + 1):
                if inside(bx, by):
                    blocks.add((bx, by))
    blocks = sorted(blocks, key=lambda b: (b[1], b[0]))
    json.dump(blocks, open(path, "w"))
    print("swiss: %d blocks" % len(blocks), flush=True)
    return blocks


SWISS_READS = ThreadPoolExecutor(24)


def swiss_tile(url):
    """A swisstopo tile at 1 m, in memory: the first overview, its origin moved
    (see swiss_layer), or the whole tile when it has no exact half."""
    from osgeo import gdal

    gdal.UseExceptions()
    # The first overview: every other sample, from the north-west one of each 2
    # x 2, so its centres are a quarter metre north-west of where the file
    # says. A tile cut short at the border (2000 x 1731) has no exact half, and
    # a smaller scrap (464 x 377) no overview: they are read whole.
    full = gdal.Open("/vsicurl/" + url)
    gt = list(full.GetGeoTransform())
    assert gt[1] == 0.5 and gt[5] == -0.5, (url, gt)
    src = full
    if full.GetRasterBand(1).GetOverviewCount() > 0:
        ov = gdal.OpenEx("/vsicurl/" + url, open_options=["OVERVIEW_LEVEL=0"])
        if (2 * ov.RasterXSize, 2 * ov.RasterYSize) == (full.RasterXSize, full.RasterYSize):
            src, gt = ov, [gt[0] - 0.25, 1, 0, gt[3] + 0.25, 0, -1]
    band = src.GetRasterBand(1)
    mem = gdal.GetDriverByName("MEM").Create("", src.RasterXSize, src.RasterYSize, 1, band.DataType)
    mem.SetGeoTransform(gt)
    mem.SetProjection(full.GetProjection())
    out = mem.GetRasterBand(1)
    if band.GetNoDataValue() is not None:
        out.SetNoDataValue(band.GetNoDataValue())
    out.WriteArray(band.ReadAsArray())
    return mem


def swiss_layer(urls, to_ch, x0, y0, side):
    """swisstopo's tiles at 1 m over the Lambert-93 square [x0, x0 + side) x [y0,
    y0 + side), rows from the north; NaN where they have no data. The tiles are
    read in parallel (one at a time, a block waited on the network)."""
    from osgeo import gdal

    gdal.UseExceptions()
    corners = [to_ch.TransformPoint(x, y)[:2] for x in (x0, x0 + side) for y in (y0, y0 + side)]
    e0, e1 = int(min(c[0] for c in corners) // 1000), int(max(c[0] for c in corners) // 1000)
    n0, n1 = int(min(c[1] for c in corners) // 1000), int(max(c[1] for c in corners) // 1000)
    tiles = [urls["%d-%d" % (e, n)] for e in range(e0, e1 + 1) for n in range(n0, n1 + 1)
             if "%d-%d" % (e, n) in urls]
    if not tiles:
        return np.full((side, side), np.nan, np.float32)
    srcs = list(SWISS_READS.map(swiss_tile, tiles))
    out = gdal.Warp("", srcs, format="MEM", dstSRS="EPSG:2154",
                    outputBounds=(x0, y0, x0 + side, y0 + side), xRes=1, yRes=1, resampleAlg="bilinear",
                    srcNodata=-9999, dstNodata=-9999, outputType=gdal.GDT_Float32, overviewLevel="NONE")
    a = out.ReadAsArray().astype(np.float32)
    a[a < -1000] = np.nan
    return a


def wms_square(layer, x0, y0, side, s, pool):
    """GetMaps of the Lambert-93 square at s m, in pieces of at most 2048^2."""
    pieces = next(p for p in range(1, 64) if side % (s * p) == 0 and side // s // p <= 2048)
    n = side // s // pieces
    m = side // pieces
    futs = {(qr, qc): pool.submit(wms, layer, 2154, x0 + qc * m, y0 + (pieces - 1 - qr) * m, m, m, n, n)
            for qr in range(pieces) for qc in range(pieces)}
    a = np.empty((n * pieces, n * pieces), np.float32)
    for (qr, qc), f in futs.items():
        a[qr * n:(qr + 1) * n, qc * n:(qc + 1) * n] = f.result()
    return a


def proximity(mask, limit):
    """Distance in samples to the nearest true sample, capped at limit."""
    from osgeo import gdal

    h, w = mask.shape
    mem = gdal.GetDriverByName("MEM")
    src = mem.Create("", w, h, 1, gdal.GDT_Byte)
    src.GetRasterBand(1).WriteArray(mask.astype(np.uint8))
    dst = mem.Create("", w, h, 1, gdal.GDT_Float32)
    gdal.ComputeProximity(src.GetRasterBand(1), dst.GetRasterBand(1),
                          ["VALUES=1", "DISTUNITS=PIXEL", "MAXDIST=%d" % limit, "NODATA=%d" % limit])
    return dst.GetRasterBand(1).ReadAsArray()


def blend(ign, ch, feather):
    """IGN's heights, fading to swisstopo's over the feather samples of their
    overlap next to the ground only swisstopo covers."""
    iv, sv = ~np.isnan(ign), ~np.isnan(ch)
    out = np.where(iv, ign, ch)
    only = sv & ~iv
    both = iv & sv
    if only.any() and both.any():
        w = np.clip(proximity(only, feather) / np.float32(feather), 0, 1)
        out[both] = (w * ign + (1 - w) * ch)[both]
    return out


def utm32():
    from osgeo import osr

    osr.UseExceptions()
    u, l93 = osr.SpatialReference(), osr.SpatialReference()
    u.ImportFromEPSG(32632)
    l93.ImportFromEPSG(2154)
    for s in (u, l93):
        s.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    return osr.CoordinateTransformation(l93, u)


def utm_box(to_utm, x0, y0, side, margin):
    cs = [to_utm.TransformPoint(x, y)[:2] for x in (x0, x0 + side) for y in (y0, y0 + side)]
    return (min(c[0] for c in cs) - margin, min(c[1] for c in cs) - margin,
            max(c[0] for c in cs) + margin, max(c[1] for c in cs) + margin)


def to_l93(src, x0, y0, side, nodata):
    """A GDAL dataset (or path) at 1 m over the Lambert-93 square, rows from the
    north; NaN where it has no data."""
    from osgeo import gdal

    gdal.UseExceptions()
    out = gdal.Warp("", src, format="MEM", dstSRS="EPSG:2154", outputBounds=(x0, y0, x0 + side, y0 + side),
                    xRes=1, yRes=1, resampleAlg="bilinear", srcNodata=nodata, dstNodata=-9999,
                    outputType=gdal.GDT_Float32, overviewLevel="NONE")
    a = out.ReadAsArray().astype(np.float32)
    a[a < -1000] = np.nan
    return a


def tinitaly(state, box):
    """TINITALY 1.1's 50 km tiles (10 m, UTM 32N) over the area, downloaded once
    into the state dir; the path of a VRT of them."""
    from osgeo import gdal
    import zipfile

    gdal.UseExceptions()
    d = os.path.join(state, "tinitaly")
    vrt = os.path.join(d, "tinitaly.vrt")
    if os.path.exists(vrt):
        return vrt
    os.makedirs(d, exist_ok=True)
    page = urllib.request.urlopen(TINITALY + "Download_Area1_1.html", timeout=120).read().decode("utf-8", "replace")
    # w<NNN><EE>: the 50 km square of UTM 32N from northing NNN x 10 km and
    # easting EE x 10 km (w50530's data starts at 327500, 5087500 from the top).
    lat0, lat1, lon0, lon1 = box
    from osgeo import osr

    wgs, u = osr.SpatialReference(), osr.SpatialReference()
    wgs.ImportFromEPSG(4326)
    u.ImportFromEPSG(32632)
    for sr in (wgs, u):
        sr.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    t = osr.CoordinateTransformation(wgs, u)
    pts = [t.TransformPoint(lon0 + (lon1 - lon0) * i / 20, lat)[:2] for i in range(21) for lat in (lat0, lat1)] + [
        t.TransformPoint(lon, lat0 + (lat1 - lat0) * i / 20)[:2] for i in range(21) for lon in (lon0, lon1)]
    e0, e1 = min(p[0] for p in pts), max(p[0] for p in pts)
    n0, n1 = min(p[1] for p in pts), max(p[1] for p in pts)
    tifs = []
    for name in sorted(set(re.findall(r"data_1\.1/(w\d{5})_s10/", page))):
        n, e = int(name[1:4]) * 10_000, int(name[4:6]) * 10_000
        if e + 50_000 <= e0 or e >= e1 or n + 50_000 <= n0 or n >= n1:
            continue
        tif = os.path.join(d, name + "_s10.tif")
        if not os.path.exists(tif):
            z = os.path.join(d, name + ".zip")
            for attempt in range(8):
                try:
                    with open(z, "wb") as f:
                        f.write(urllib.request.urlopen(TINITALY + "data_1.1/%s_s10/%s_s10.zip" % (name, name),
                                                       timeout=600).read())
                    break
                except Exception as ex:
                    print("  retry", attempt, repr(ex), name, flush=True)
                    time.sleep(3 * (attempt + 1))
            with zipfile.ZipFile(z) as zf:
                member = next(m for m in zf.namelist() if m.endswith(".tif"))
                with zf.open(member) as src, open(tif + ".part", "wb") as dst:
                    shutil.copyfileobj(src, dst)
            os.replace(tif + ".part", tif)
            os.remove(z)
        tifs.append(tif)
    print("tinitaly: %d tiles" % len(tifs), flush=True)
    gdal.BuildVRT(vrt, tifs, srcNodata=-9999, VRTNodata=-9999)
    return vrt


def piemonte(to_utm, x0, y0, side):
    """Regione Piemonte's 5 m DTM over the Lambert-93 square at 1 m, from its WCS."""
    from osgeo import gdal

    gdal.UseExceptions()
    e0, n0, e1, n1 = utm_box(to_utm, x0, y0, side, 20)
    e0, n0, e1, n1 = int(e0 // 5 * 5), int(n0 // 5 * 5), int(e1 // 5 * 5 + 5), int(n1 // 5 * 5 + 5)
    pe0, pn0, pe1, pn1 = PIEMONTE_BOX
    if e1 <= pe0 or e0 >= pe1 or n1 <= pn0 or n0 >= pn1:
        return np.full((side, side), np.nan, np.float32)
    url = (PIEMONTE + "?service=WCS&version=1.0.0&request=GetCoverage&coverage=DTM&crs=EPSG:32632"
           "&bbox=%d,%d,%d,%d&resx=5&resy=5&format=image/tiff" % (e0, n0, e1, n1))
    for attempt in range(8):
        try:
            d = urllib.request.urlopen(url, timeout=300).read()
            if d[:4] in (b"II*\0", b"MM\0*"):
                break
            if b"does not intersect" in d or b"outside requested coverage" in d:  # beyond its bounding box
                return np.full((side, side), np.nan, np.float32)
            err = d[:200]
        except Exception as ex:
            err = repr(ex)
        print("  retry", attempt, err, "piemonte", x0, y0, flush=True)
        time.sleep(3 * (attempt + 1))
    else:
        raise RuntimeError("gave up on %s" % url)
    mem = "/vsimem/piemonte_%d_%d_%d.tif" % (x0, y0, threading.get_ident())
    gdal.FileFromMemBuffer(mem, d)
    try:
        return to_l93(mem, x0, y0, side, -99)
    finally:
        gdal.Unlink(mem)


def plan_italy(state, vrt):
    """The lamb93 blocks within the area that TINITALY covers, plus neighbours."""
    path = os.path.join(state, "blocks.json")
    if os.path.exists(path):
        return [tuple(b) for b in json.load(open(path))]
    from osgeo import gdal

    gdal.UseExceptions()
    (bx0, bx1, by0, by1), inside, _, _, _ = area("lamb93")
    nx, ny = bx1 - bx0 + 1, by1 - by0 + 1
    cov = gdal.Warp("", vrt, format="MEM", dstSRS="EPSG:2154", outputBounds=(bx0 * B, by0 * B, (bx1 + 1) * B, (by1 + 1) * B),
                    xRes=256, yRes=256, resampleAlg="average", srcNodata=-9999, dstNodata=-9999).ReadAsArray()
    has = (cov > -1000).reshape(ny, 16, nx, 16).any(axis=(1, 3))[::-1]
    grown = has.copy()
    grown[1:] |= has[:-1]
    grown[:-1] |= has[1:]
    grown[:, 1:] |= grown[:, :-1].copy()
    grown[:, :-1] |= grown[:, 1:].copy()
    blocks = [(bx0 + i, by0 + j) for j in range(ny) for i in range(nx) if grown[j, i] and inside(bx0 + i, by0 + j)]
    json.dump(blocks, open(path, "w"))
    print("italy: %d blocks" % len(blocks), flush=True)
    return blocks


def build_block_abroad(bx, by, root, pool, swiss, italy, pad, ign=None):
    """build_block for a block another country's data touches: IGN's, then
    swisstopo's ((index, to_ch) or None), then the Italian models ((TINITALY's
    VRT, to_utm) or None), each blended into the one before. IGN's are fetched
    only for the blocks in ign, if given: plan_blocks's, which includes their
    neighbours (21 of an Italian block's 30 s went to IGN's empty GetMaps)."""
    x0, y0, side = bx * B - pad, by * B - pad, B + 2 * pad
    fp = pool.submit(piemonte, italy[1], x0, y0, side) if italy else None
    if ign is None or (bx, by) in ign:
        fg = pool.submit(wms_square, MNT + ".LAMB93", x0, y0, side, 1, pool)
        fc = pool.submit(wms_square, MNH + ".LAMB93", x0, y0, side, 2, pool)
        g, c = fg.result(), fc.result()
    else:
        g = np.full((side, side), np.nan, np.float32)
        c = np.full((side // 2, side // 2), np.nan, np.float32)
    if swiss:
        index, to_ch = swiss
        ground = swiss_layer(index["ground"], to_ch, x0, y0, side)
        canopy = halve(swiss_layer(index["surface"], to_ch, x0, y0, side)
                       - swiss_layer(index["under"], to_ch, x0, y0, side))
        g = blend(g, ground, FEATHER)
        c = blend(c, canopy, FEATHER // 2)
    fine = g
    if italy:
        it = blend(fp.result(), to_l93(italy[0], x0, y0, side, -9999), FEATHER)
        g = blend(g, it, ITALY_FEATHER)
        fine = np.where(np.isnan(fine), np.float32(np.nan), g)
    fine, g = fine[pad:-pad, pad:-pad], g[pad:-pad, pad:-pad]
    c = c[pad // 2:-pad // 2, pad // 2:-pad // 2]
    files = []
    write_level(root, "ground", 1, fine, bx, by, files)
    for s in (2, 4, 8):
        g = halve(g)
        if s in WRITE["ground"]:
            write_level(root, "ground", s, g, bx, by, files)
    for s in (2, 4, 8, 16):
        if s in WRITE["canopy"]:
            write_level(root, "canopy", s, c, bx, by, files)
        if s < 16:
            c = halve(c)
    return files


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
    ap.add_argument("--swiss", action="store_true", help="the blocks swisstopo covers, from both countries")
    ap.add_argument("--italy", action="store_true", help="the blocks TINITALY covers, from all three")
    args = ap.parse_args()
    if bool(args.out) == bool(args.bucket):
        ap.error("give exactly one of --out and --bucket")
    if (args.swiss or args.italy) and args.region != "lamb93":
        ap.error("--swiss and --italy go with lamb93")
    if args.swiss and args.italy:
        ap.error("give one of --swiss and --italy")
    os.makedirs(args.state, exist_ok=True)
    if args.swiss or args.italy:
        # GDAL reads swisstopo's GeoTIFFs over HTTP, a few ranges of each.
        os.environ.update(GDAL_DISABLE_READDIR_ON_OPEN="EMPTY_DIR", GDAL_HTTP_MAX_RETRY="8",
                          GDAL_HTTP_RETRY_DELAY="3", GDAL_HTTP_TIMEOUT="120")
        box = area("lamb93")[2]
        swiss = (swiss_index(args.state, box), lv95()[1])
        if args.italy:
            italy = (tinitaly(args.state, box), utm32())
            planned, pad = plan_italy(args.state, italy[0]), ITALY_PAD
            # IGN's blocks, planned as for the run that built them.
            os.makedirs(os.path.join(args.state, "ign"), exist_ok=True)
            ign = set(plan_blocks("lamb93", os.path.join(args.state, "ign")))
        else:
            italy, ign = None, None
            planned, pad = plan_swiss(args.state), PAD

        def build(region, bx, by, root, pool):
            return build_block_abroad(bx, by, root, pool, swiss, italy, pad, ign)
    else:
        planned = None
        build = build_block
    todo = list(zip(args.blocks[::2], args.blocks[1::2])) or planned or plan_blocks(args.region, args.state)
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
            files = build(args.region, bx, by, root, fetch_pool)
            uploader.upload(files, staging, args.prefix)
            shutil.rmtree(staging)
        else:
            root = os.path.join(args.out, args.prefix, args.region)
            files = build(args.region, bx, by, root, fetch_pool)
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
