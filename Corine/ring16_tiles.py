"""The 16 m ring: bundles of 16 m ground tiles, each the 2 x 2 mean of the 8 m
tiles own_tiles.py built (nodata where any of the four is), between the 8 m
ring and the base.

    ring16_tiles.py <region> <state dir>... [--source URL] [--jobs N] [--uploads N]
                    [--fetches N] [--step M] (--out DIR | --bucket B --endpoint URL)
                    [--prefix v2] [--only C R ...] [--done FILE]

A bundle is 8192 m, the step the client snaps a 2048-sample 16 m block to
(Hd_dem.projected_block): 2 x 2 tiles of 4096 m, ground only, in the HDB1
layout of ring_bundles.py, at <prefix>/rings/<region>/16/<C>_<R>.hdb, C and R
counting 8192 m from the CRS origin (R northwards). Heights are in --step
steps, 10 cm by default: finer than the base's 14.5 cm, and a bundle a third
smaller than at the 8 m ring's 1 cm.

The bundles are those over the blocks the state dirs' done.jsonl list with
files (the runs that built the tiles, see own_tiles.py), 2 x 2 blocks a
bundle; --only C R ... builds these bundles instead. The 8 m tiles are read
from --source (the public https://tiles.vouillon.org/v1 by default), 16 a
bundle. Each bundle built is logged in ring16-<region>-<prefix>.done (--done)
-- a rerun resumes; one whose tiles still fail after the retries is skipped
and said so, and the run exits 1 at the end for a rerun to do it. The state
dirs are only read. Needs boto3 to upload, credentials in AWS_ACCESS_KEY_ID
and AWS_SECRET_ACCESS_KEY.
"""
import argparse, json, os, struct, sys, threading, time, zlib
from concurrent.futures import ThreadPoolExecutor
import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from own_tiles import T, encode_ground, halve  # noqa: E402
from ring_bundles import fetch  # noqa: E402

BUNDLE = 8192
BLOCK = 4096


def decode(d):
    """Heights of an HDT1 tile, NaN where nodata."""
    w, h, base, step, n1, n2 = struct.unpack('<IIffII', d[4:28])
    hi = np.frombuffer(zlib.decompress(d[28:28 + n1]), np.uint8).astype(np.int64)
    lo = np.frombuffer(zlib.decompress(d[28 + n1:28 + n1 + n2]), np.uint8).astype(np.int64)
    z = (hi << 8) | lo
    r = (z >> 1) ^ -(z & 1)
    q = np.cumsum(np.cumsum(r.reshape(h, w), 0), 1) & 0xFFFF
    out = (np.float32(base) + np.float32(step) * q.astype(np.float32)).astype(np.float64)
    out[q == 0xFFFF] = np.nan
    return out


def bundle(source, region, C, R, pool, step):
    """The bundle (C, R) and the 8 m tiles it was made from; None if none."""
    # Its 4 x 4 8 m tiles, row 0 north as in a raster.
    want = [(c, r) for r in range(4 * R, 4 * R + 4) for c in range(4 * C, 4 * C + 4)]
    got = dict(zip(want, pool.map(lambda w: fetch(source, region, 'ground', 8, *w), want)))
    ntiles = sum(1 for d in got.values() if d)
    if not ntiles:
        return None, 0
    a = np.full((4 * T, 4 * T), np.nan)
    for (c, r), d in got.items():
        if d:
            i = 3 - (r - 4 * R)
            j = c - 4 * C
            a[i * T:(i + 1) * T, j * T:(j + 1) * T] = decode(d)
    h = halve(a)  # 2 x 2 tiles of 16 m
    parts = []
    for dy in range(2):
        for dx in range(2):
            i = 1 - dy
            blob = encode_ground(h[i * T:(i + 1) * T, dx * T:(dx + 1) * T], step)
            if blob:
                parts.append((0, dx + 2 * dy, blob))
    if not parts:
        return None, ntiles
    head = b'HDB1' + bytes([len(parts)]) + b''.join(struct.pack('<BBI', p, s, len(d)) for p, s, d in parts)
    return head + b''.join(d for _, _, d in parts), ntiles


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('region'); ap.add_argument('state', nargs='+')
    ap.add_argument('--source', default='https://tiles.vouillon.org/v1')
    ap.add_argument('--out'); ap.add_argument('--bucket'); ap.add_argument('--endpoint')
    ap.add_argument('--prefix', default='v2'); ap.add_argument('--jobs', type=int, default=16)
    ap.add_argument('--uploads', type=int, default=32, help='bundles uploaded at once')
    ap.add_argument('--fetches', type=int, default=64, help='tiles fetched at once, over all jobs')
    ap.add_argument('--step', type=float, default=0.1)
    ap.add_argument('--only', nargs='*', type=int, help='C R ... : these bundles only')
    ap.add_argument('--done', help='the log of bundles done (default ring16-<region>-<prefix>.done here)')
    a = ap.parse_args()
    if bool(a.out) == bool(a.bucket): ap.error('give exactly one of --out and --bucket')
    want = set()
    for d in a.state:
        for l in open(os.path.join(d, 'done.jsonl')):
            j = json.loads(l)
            if j['files'] > 0: want.add((j['bx'] * BLOCK // BUNDLE, j['by'] * BLOCK // BUNDLE))
    if a.only: want = {(a.only[i], a.only[i + 1]) for i in range(0, len(a.only), 2)}
    done_path = a.done or f'ring16-{a.region}-{a.prefix}.done'
    done = set()
    if os.path.exists(done_path): done = {tuple(map(int, l.split()[:2])) for l in open(done_path)}
    todo = sorted(want - done)
    print(f'{a.region}: {len(want)} bundles, {len(todo)} to do', flush=True)
    s3 = None
    if a.bucket:
        import boto3
        from botocore.config import Config
        s3 = boto3.client('s3', endpoint_url=a.endpoint,
                          config=Config(max_pool_connections=a.uploads + 8,
                                        retries={'max_attempts': 10, 'mode': 'adaptive'}))
    lock = threading.Lock(); stats = {'bundles': 0, 'written': 0, 'tiles': 0, 'bytes': 0}; t0 = time.time()
    tiles_pool = ThreadPoolExecutor(a.fetches)
    failed = []

    def one(b):
        C, R = b
        try:
            data, ntiles = bundle(a.source, a.region, C, R, tiles_pool, a.step)
            if data:
                key = f'{a.prefix}/rings/{a.region}/16/{C}_{R}.hdb'
                if s3:
                    s3.put_object(Bucket=a.bucket, Key=key, Body=data, ContentType='application/octet-stream',
                                  CacheControl='public, max-age=31536000, immutable')
                else:
                    p = os.path.join(a.out, key); os.makedirs(os.path.dirname(p), exist_ok=True)
                    with open(p + '.tmp', 'wb') as f: f.write(data)
                    os.replace(p + '.tmp', p)
        except Exception as e:
            with lock: failed.append(b)
            print(f'bundle {C} {R} failed, left for a rerun: {e}', flush=True)
            return
        with lock:
            stats['bundles'] += 1; stats['tiles'] += ntiles
            if data: stats['written'] += 1; stats['bytes'] += len(data)
            with open(done_path, 'a') as f: f.write(f'{C} {R} {len(data) if data else 0} {ntiles}\n')
            if stats['bundles'] % 100 == 0 or stats['bundles'] == len(todo):
                el = time.time() - t0
                print(f"{stats['bundles']}/{len(todo)} bundles, {stats['written']} written from {stats['tiles']} tiles, "
                      f"{stats['bytes'] / 1e6:.0f} MB, {el / 60:.0f} min, "
                      f"~{el / stats['bundles'] * (len(todo) - stats['bundles']) / 60:.0f} min to go", flush=True)

    with ThreadPoolExecutor(min(a.jobs, a.uploads)) as pool:
        for _ in pool.map(one, todo): pass
    if failed:
        print(f'{len(failed)} bundles failed: run the same command again to do them', flush=True)
        sys.exit(1)


if __name__ == '__main__':
    main()
