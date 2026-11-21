"""Bundle the rings' tiles (own_tiles.py) four ground tiles and their canopy
a file, so that a location asks for 48 files rather than ~190.

    ring_bundles.py <region> <state dir>... [--source URL] [--jobs N] [--uploads N] [--fetches N]
                    (--out DIR | --bucket B --endpoint URL) [--prefix v2]

Each ground ring has a canopy over the same ground: 1 m with 2 m, 4 m with
4 m, 8 m with 16 m. A bundle covers the step the client snaps that ring's
block to (a quarter of its span, Hd_dem.projected_block), so that a block is
whole bundles: for the 1 m ring 512 m (its 2 x 2 ground tiles and the one 2 m
canopy tile there), for the 4 m ring 1024 m (one ground tile, one canopy
tile), for the 8 m ring 4096 m, a block (2 x 2 ground tiles, the one 16 m
canopy tile). It is
<prefix>/rings/<region>/<ring m>/<C>_<R>.hdb, C and R counting bundles as
own_tiles.py counts tiles (x and y over the bundle's side, R northwards):

  0 "HDB1"  4 count u8, then per part: part u8 (0 ground, 1 canopy), sub u8
  (dx + 2 dy: the tile (k C + dx, k R + dy) of its product, k tiles a side of
  that part in the bundle, 0 for a single tile), length u32 LE; then the
  parts' HDT1 tiles as they are.

A tile that does not exist is left out, and a bundle with none is not
written: data bit-identical to the tiles, nothing re-encoded.

The blocks are the ones the state dirs' done.jsonl list with files (the runs
that built the tiles, see own_tiles.py); the tiles are read from --source
(the public https://tiles.vouillon.org/v1 by default). Block by block: fetch
its 357 tiles, write its 81 bundles at most, upload them, log it in
bundles-<region>-<prefix>.done (--done) -- a rerun resumes. A block whose
tiles still fail after the retries is skipped and said so; the run goes on,
and exits 1 at the end for a rerun to do those. The state dirs are
only read. Needs boto3
to upload, credentials in AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY.
"""
import argparse, json, os, struct, sys, threading, time, urllib.error, urllib.request
from concurrent.futures import ThreadPoolExecutor

# ring (ground m): (canopy m, ground tiles a side of a bundle, canopy tiles a side)
RINGS = {1: (2, 2, 1), 4: (4, 1, 1), 8: (16, 2, 1)}
BLOCK = 4096
TILE = 256

def fetch(source, region, product, m, c, r, tries=10):
    url = f'{source}/{region}/{product}/{m}/{c}_{r}.hdt'
    for k in range(tries):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers={'User-Agent': 'tile-builder'}), timeout=60) as f:
                d = f.read()
            if d[:4] != b'HDT1': raise ValueError('not HDT1')
            return d
        except urllib.error.HTTPError as e:
            if e.code == 404: return None
            err = e
        except Exception as e:
            err = e
        # Cloudflare's 5xx (520 seen) come in spells: back off to a minute, ~6 min in all.
        time.sleep(min(60, 2 ** (k + 1)))
    raise RuntimeError(f'{url}: {err}')

def bundles(source, region, bx, by, pool):
    """The block's bundles: [(ring, C, R, bytes)]."""
    want = []  # (product, m, c, r)
    for g, (cm, _, _) in RINGS.items():
        n = BLOCK // (TILE * g)              # ground tiles a side in the block
        for r in range(by * n, by * n + n):
            for c in range(bx * n, bx * n + n): want.append(('ground', g, c, r))
        n = max(1, BLOCK // (TILE * cm))     # canopy tiles a side
        for r in range(by * n, by * n + n):
            for c in range(bx * n, bx * n + n): want.append(('canopy', cm, c, r))
    got = dict(zip(want, pool.map(lambda w: fetch(source, region, *w), want)))
    out = []
    for g, (cm, kg, kc) in RINGS.items():
        side = BLOCK // (kg * TILE * g)      # bundles a side in the block
        for R in range(by * side, by * side + side):
            for C in range(bx * side, bx * side + side):
                parts = []
                for part, product, m, k in ((0, 'ground', g, kg), (1, 'canopy', cm, kc)):
                    for dy in range(k):
                        for dx in range(k):
                            d = got.get((product, m, k * C + dx, k * R + dy))
                            if d: parts.append((part, dx + 2 * dy, d))
                if not parts: continue
                head = b'HDB1' + bytes([len(parts)]) + b''.join(struct.pack('<BBI', p, s, len(d)) for p, s, d in parts)
                out.append((g, C, R, head + b''.join(d for _, _, d in parts)))
    return out, sum(1 for v in got.values() if v)

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('region'); ap.add_argument('state', nargs='+')
    ap.add_argument('--source', default='https://tiles.vouillon.org/v1')
    ap.add_argument('--out'); ap.add_argument('--bucket'); ap.add_argument('--endpoint')
    ap.add_argument('--prefix', default='v2'); ap.add_argument('--jobs', type=int, default=4)
    ap.add_argument('--uploads', type=int, default=32, help='bundles uploaded at once, over all jobs')
    ap.add_argument('--fetches', type=int, default=64, help='tiles fetched at once, over all jobs')
    ap.add_argument('--only', nargs='*', type=int, help='bx by ... : these blocks only')
    ap.add_argument('--done', help='the log of blocks done (default bundles-<region>-<prefix>.done here)')
    a = ap.parse_args()
    if bool(a.out) == bool(a.bucket): ap.error('give exactly one of --out and --bucket')
    blocks = set()
    for d in a.state:
        for l in open(os.path.join(d, 'done.jsonl')):
            j = json.loads(l)
            if j['files'] > 0: blocks.add((j['bx'], j['by']))
    if a.only: blocks = {(a.only[i], a.only[i + 1]) for i in range(0, len(a.only), 2)}
    done_path = a.done or f'bundles-{a.region}-{a.prefix}.done'
    done = set()
    if os.path.exists(done_path): done = {tuple(map(int, l.split()[:2])) for l in open(done_path)}
    todo = sorted(blocks - done)
    print(f'{a.region}: {len(blocks)} blocks, {len(todo)} to do', flush=True)
    s3 = None
    if a.bucket:
        import boto3
        from botocore.config import Config
        s3 = boto3.client('s3', endpoint_url=a.endpoint,
                          config=Config(max_pool_connections=a.uploads + 8,
                                        retries={'max_attempts': 10, 'mode': 'adaptive'}))
    lock = threading.Lock(); stats = {'blocks': 0, 'bundles': 0, 'tiles': 0, 'bytes': 0}; t0 = time.time()
    tiles_pool = ThreadPoolExecutor(a.fetches)
    # A block's bundles go up together: one after the other, at ~150 ms a PUT,
    # its 81 took 12 s and held the whole run to ~35 Mbit/s.
    upload_pool = ThreadPoolExecutor(a.uploads)
    failed = []
    def one(b):
        bx, by = b
        # A block that fails is reported and left out of the log, for a rerun to
        # do again; the others go on.
        try:
            out, ntiles = bundles(a.source, a.region, bx, by, tiles_pool)
            def put(bundle):
                g, C, R, data = bundle
                key = f'{a.prefix}/rings/{a.region}/{g}/{C}_{R}.hdb'
                if s3:
                    s3.put_object(Bucket=a.bucket, Key=key, Body=data, ContentType='application/octet-stream',
                                  CacheControl='public, max-age=31536000, immutable')
                else:
                    p = os.path.join(a.out, key); os.makedirs(os.path.dirname(p), exist_ok=True)
                    with open(p + '.tmp', 'wb') as f: f.write(data)
                    os.replace(p + '.tmp', p)
            for _ in upload_pool.map(put, out): pass
        except Exception as e:
            with lock: failed.append(b)
            print(f'block {bx} {by} failed, left for a rerun: {e}', flush=True)
            return
        with lock:
            stats['blocks'] += 1; stats['bundles'] += len(out); stats['tiles'] += ntiles
            stats['bytes'] += sum(len(d) for *_, d in out)
            with open(done_path, 'a') as f: f.write(f'{bx} {by} {len(out)} {ntiles}\n')
            if stats['blocks'] % 25 == 0 or stats['blocks'] == len(todo):
                el = time.time() - t0
                print(f"{stats['blocks']}/{len(todo)} blocks, {stats['bundles']} bundles from {stats['tiles']} tiles, "
                      f"{stats['bytes'] / 1e9:.2f} GB, {el / 60:.0f} min, "
                      f"~{el / stats['blocks'] * (len(todo) - stats['blocks']) / 60:.0f} min to go", flush=True)
    with ThreadPoolExecutor(a.jobs) as pool:
        for _ in pool.map(one, todo): pass
    if failed:
        print(f'{len(failed)} blocks failed: run the same command again to do them', flush=True)
        sys.exit(1)

if __name__ == '__main__':
    main()
