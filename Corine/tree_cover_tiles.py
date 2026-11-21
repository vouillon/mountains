"""Our own tree cover tiles, from the EEA's public image service (HRL Tree
Cover Density and Dominant Leaf Type, 2018), as the app asks for them: WGS84,
bilinear density, nearest leaf type.

    python3 Corine/tree_cover_tiles.py <out dir> <level> <lat0> <lat1> <lon0> <lon1> [--jobs 4]

The current tiles: level 1 over 41.4-47.6 N, 3.4-10.6 E; level 0 over
41.9-47.1 N, 3.9-10.1 E (5078 + 507 cells, 220 MB, 2026-11-21). A rerun
resumes from <out>/tc<level>.done.

level 0: cells of 300" at 0.6" (500 x 500), level 1: cells of 1200" at 2.4".
A cell is <out>/tc<level>/N45_E006_<row>_<col>.tc, row 0 south, col 0 west:
b"TCC1" and the zlib of its 500 x 500 bytes, rows from the north, each the
density in steps of 10% (0-10; 15 no data) in the low 4 bits and the leaf
type (0 neither, 1 broadleaved, 2 coniferous) in the next 2. A cell with no
data at all is not written. Fetched in blocks of 8 x 8 cells.
"""
import sys, os, struct, zlib, time, urllib.request, argparse
from concurrent.futures import ThreadPoolExecutor
import numpy as np

N = 500          # samples per cell side
BLOCK = 8        # cells per block side
SERVICE = 'https://image.discomap.eea.europa.eu/arcgis/rest/services/GioLandPublic/{}/ImageServer/exportImage'

def read_tiff(d):
    assert d[:4] == b'II*\x00', d[:64]
    ifd = struct.unpack('<I', d[4:8])[0]; n = struct.unpack('<H', d[ifd:ifd + 2])[0]; tags = {}
    for k in range(n):
        t, ty, c, v = struct.unpack('<HHII', d[ifd + 2 + 12 * k: ifd + 14 + 12 * k])
        if c == 1: tags[t] = v & 0xFFFF if ty == 3 else v
        elif ty in (3, 4):
            tags[t] = list(struct.unpack('<%d%s' % (c, 'H' if ty == 3 else 'I'), d[v:v + c * (2 if ty == 3 else 4)]))
    w, h = tags[256], tags[257]; img = np.zeros((h, w), np.uint8)
    if 322 in tags:
        tw, th = tags[322], tags[323]; offs = tags[324] if isinstance(tags[324], list) else [tags[324]]
        across = (w + tw - 1) // tw
        for i, o in enumerate(offs):
            if o == 0: continue
            r, c = divmod(i, across)
            t = np.frombuffer(d[o:o + tw * th], np.uint8).reshape(th, tw)
            hh, ww = min(th, h - r * th), min(tw, w - c * tw)
            img[r * th:r * th + hh, c * tw:c * tw + ww] = t[:hh, :ww]
    else:
        offs = tags[273] if isinstance(tags[273], list) else [tags[273]]
        cnt = tags[279] if isinstance(tags[279], list) else [tags[279]]
        img = np.frombuffer(b''.join(d[o:o + c] for o, c in zip(offs, cnt))[:w * h], np.uint8).reshape(h, w)
    return img

def fetch(svc, interp, w, s, e, n, px):
    url = (SERVICE.format(svc) + f'?bbox={w:.8f},{s:.8f},{e:.8f},{n:.8f}&bboxSR=4326&imageSR=4326'
           f'&size={px},{px}&format=tiff&pixelType=U8&interpolation={interp}&compression=None&f=image')
    for attempt in range(6):
        try:
            d = urllib.request.urlopen(urllib.request.Request(url, headers={'User-Agent': 'tile-builder'}), timeout=300).read()
            img = read_tiff(d)
            if img.shape == (px, px): return img
            raise ValueError(f'shape {img.shape}')
        except Exception as ex:
            print(f'  retry {attempt + 1} ({ex})', file=sys.stderr, flush=True); time.sleep(5 * (attempt + 1))
    raise RuntimeError('gave up on ' + url)

def name(level, row, col):  # row, col: cell indices from 0 deg
    per = 12 if level == 0 else 3
    lat, lon = row // per, col // per
    return f'{"N" if lat >= 0 else "S"}{abs(lat):02d}_{"E" if lon >= 0 else "W"}{abs(lon):03d}_{row - per * lat}_{col - per * lon}.tc'

def block(out, level, r0, c0):
    cell = (300 if level == 0 else 1200) / 3600
    px = N * BLOCK
    w, s = c0 * cell, r0 * cell; e, n = w + BLOCK * cell, s + BLOCK * cell
    t = time.time()
    d = fetch('HRL_TreeCoverDensity_2018', 'RSP_BilinearInterpolation', w, s, e, n, px).astype(np.int32)
    l = fetch('HRL_DominantLeafType2018', 'RSP_NearestNeighbor', w, s, e, n, px).astype(np.int32)
    q = np.where(d > 100, 15, np.round(d / 10)).astype(np.int32)
    b = (q | (np.where(l <= 2, l, 0) << 4)).astype(np.uint8)
    written = bytes_ = 0
    for i in range(BLOCK):          # i: cell row from the south
        for j in range(BLOCK):
            # rows of the response run from the north
            sub = b[(BLOCK - 1 - i) * N:(BLOCK - i) * N, j * N:(j + 1) * N]
            if (sub & 15).min() == 15: continue   # no data at all
            data = b'TCC1' + zlib.compress(sub.tobytes(), 9)
            p = os.path.join(out, f'tc{level}', name(level, r0 + i, c0 + j))
            with open(p + '.tmp', 'wb') as f: f.write(data)
            os.replace(p + '.tmp', p); written += 1; bytes_ += len(data)
    return r0, c0, written, bytes_, time.time() - t

def main():
    ap = argparse.ArgumentParser(); ap.add_argument('out'); ap.add_argument('level', type=int)
    for k in ('lat0', 'lat1', 'lon0', 'lon1'): ap.add_argument(k, type=float)
    ap.add_argument('--jobs', type=int, default=4); a = ap.parse_args()
    os.makedirs(os.path.join(a.out, f'tc{a.level}'), exist_ok=True)
    per = 12 if a.level == 0 else 3
    import math
    rows = range(math.floor(a.lat0 * per) // BLOCK * BLOCK, math.ceil(a.lat1 * per), BLOCK)
    cols = range(math.floor(a.lon0 * per) // BLOCK * BLOCK, math.ceil(a.lon1 * per), BLOCK)
    blocks = [(r, c) for r in rows for c in cols]
    # Skip blocks already done (all their cells written, or none to write): a rerun resumes.
    done_file = os.path.join(a.out, f'tc{a.level}.done'); done = set()
    if os.path.exists(done_file): done = {tuple(map(int, l.split())) for l in open(done_file)}
    todo = [b for b in blocks if b not in done]
    print(f'level {a.level}: {len(blocks)} blocks, {len(todo)} to do', flush=True)
    tot_w = tot_b = 0
    with ThreadPoolExecutor(a.jobs) as pool, open(done_file, 'a') as df:
        for k, (r0, c0, wr, by, dt) in enumerate(pool.map(lambda rc: block(a.out, a.level, *rc), todo)):
            tot_w += wr; tot_b += by; df.write(f'{r0} {c0}\n'); df.flush()
            print(f'{k + 1}/{len(todo)} block {r0},{c0}: {wr} cells, {by / 1e3:.0f} kB, {dt:.0f} s; total {tot_w} cells, {tot_b / 1e6:.1f} MB', flush=True)

main()
