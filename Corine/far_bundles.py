"""Bundle the far field's two grids into one file per sub-tile.

    python3 Corine/far_bundles.py <dem4 dir> <clc4 dir> <out dir>

The far field's elevation (compress_dem --coarse, dem4) and land cover
(extract_tiles --coarse, clc4) are DEM1 tiles on the same 1200" sub-tiles,
fetched together by every load: one request for both rather than two. A
bundle <out>/N45_E006_<row>_<col>.far is b"FAR1", the lengths of the two
tiles (u32, little-endian), then the elevation tile and the land cover tile
as they are; a length of 0 for one that does not exist (open sea). Written for
every sub-tile that has either.
"""
import os, struct, sys

def main():
    dem, clc, out = sys.argv[1:4]
    os.makedirs(out, exist_ok=True)
    names = sorted(set(os.listdir(dem)) | set(os.listdir(clc)))
    total = 0
    for name in names:
        if not name.endswith('.dem'): continue
        parts = []
        for d in (dem, clc):
            p = os.path.join(d, name)
            parts.append(open(p, 'rb').read() if os.path.exists(p) else b'')
        for b in parts: assert b == b'' or b[:4] == b'DEM1', name
        data = b'FAR1' + struct.pack('<II', len(parts[0]), len(parts[1])) + parts[0] + parts[1]
        p = os.path.join(out, name[:-4] + '.far')
        with open(p + '.tmp', 'wb') as f: f.write(data)
        os.replace(p + '.tmp', p); total += len(data)
    print(f'{len(names)} bundles, {total / 1e6:.1f} MB')

main()
