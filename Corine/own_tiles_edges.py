#!/usr/bin/env python3
"""The published blocks of an own_tiles.py run that border IGN's nodata.

    own_tiles_edges.py <base url> <region> <run state dir> <new state dir>
                       [--exclude blocks.json ...]

Until own_tiles.wms dropped them, every block that has an edge of IGN's data (a
border, the coast, a hole) carried the service's mixtures of heights with its
nodata along it: trenches a sample wide, down to -1000 m. This lists the blocks
to rebuild: those of the run (<run state dir>/blocks.json) whose published 8 m
ground or 16 m canopy, read from <base url>/<region>/..., has a nodata sample or
a missing tile, with their neighbours (an edge can run along a block's side),
but not those the run left empty (done.jsonl) nor those in the --exclude lists
(blocks another run rebuilds). The list goes to <new state dir>/blocks.json, for
own_tiles.py to rebuild just those:

    own_tiles.py <new state dir> <region> --bucket ... --endpoint ...

(a state dir's blocks.json is its plan). Needs numpy.
"""
import argparse, json, os, struct, sys, time, urllib.error, urllib.request, zlib
from concurrent.futures import ThreadPoolExecutor
import numpy as np


def has_nodata(url):
    """Whether the HDT1 tile at url has a nodata sample; None if it is absent."""
    for attempt in range(8):
        try:
            d = urllib.request.urlopen(urllib.request.Request(url, headers={"User-Agent": "own_tiles_edges"}),
                                       timeout=60).read()
            break
        except urllib.error.HTTPError as e:
            if e.code == 404:
                return None
            err = e
        except OSError as e:
            err = e
        time.sleep(2 * (attempt + 1))
    else:
        raise RuntimeError("gave up on %s: %r" % (url, err))
    w, h, _, _, n1, n2 = struct.unpack("<IIffII", d[4:28])
    z = (np.frombuffer(zlib.decompress(d[28:28 + n1]), np.uint8).astype(np.int64) << 8) | np.frombuffer(
        zlib.decompress(d[28 + n1:28 + n1 + n2]), np.uint8)
    r = (z >> 1) ^ -(z & 1)
    u = np.cumsum(np.cumsum(r.reshape(h, w), 0), 1) & 0xFFFF
    return bool((u == 65535).any())


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("base_url")
    ap.add_argument("region")
    ap.add_argument("run_state")
    ap.add_argument("new_state")
    ap.add_argument("--exclude", nargs="*", default=[])
    args = ap.parse_args()
    load = lambda p: {tuple(b) for b in json.load(open(p))}
    plan = load(os.path.join(args.run_state, "blocks.json"))
    files = {(j["bx"], j["by"]): j["files"] for j in map(json.loads, open(os.path.join(args.run_state, "done.jsonl")))}
    base = "%s/%s/" % (args.base_url.rstrip("/"), args.region)

    def edge(b):
        bx, by = b
        rels = ["ground/8/%d_%d.hdt" % (bx * 2 + i, by * 2 + j) for i in (0, 1) for j in (0, 1)]
        rels.append("canopy/16/%d_%d.hdt" % (bx, by))
        return b, any(v is None or v for v in (has_nodata(base + r) for r in rels))

    built = [b for b in plan if files.get(b, 0) > 0]
    with ThreadPoolExecutor(32) as ex:
        edges = {b for b, e in ex.map(edge, built) if e}
    todo = {(bx + i, by + j) for bx, by in edges for i in (-1, 0, 1) for j in (-1, 0, 1)}
    todo = {b for b in todo if files.get(b, 0) > 0}
    for p in args.exclude:
        todo -= load(p)
    os.makedirs(args.new_state, exist_ok=True)
    json.dump(sorted(todo, key=lambda b: (b[1], b[0])), open(os.path.join(args.new_state, "blocks.json"), "w"))
    print("%d of %d built blocks have an edge; %d to rebuild" % (len(edges), len(built), len(todo)))


if __name__ == "__main__":
    sys.exit(main())
