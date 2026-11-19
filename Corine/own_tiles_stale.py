#!/usr/bin/env python3
"""The tiles a rebuild of their block left behind in the bucket.

    own_tiles_stale.py --bucket B --endpoint E REGION=STATE_DIR ... [--delete]

e.g. lamb93=$HOME/mountains-tiles/state-lamb93 lamb93=.../state-swiss ...
rgr92utm40s=.../state-reunion ...: every run that uploaded to the bucket.

A run of own_tiles.py uploads the tiles a block has data in, overwriting those
already there; it deletes nothing. A block rebuilt with less data (the edge
repairs, once wms dropped IGN's mixtures at its nodata, emptied a few tiles
along borders and coasts) keeps its old tiles where the new build has none:
there they still show the old trenches. This lists the bucket and, in each block
(4096 m, per region) that more than one run built (from the runs' done.jsonl),
reports the tiles uploaded more than --margin seconds before the block's newest
one: every run rebuilds whole blocks, and a block's tiles go up within minutes.
Not always: an upload stalled 15 hours once (the machine asleep?), so a block
that one run built is never touched, and a rebuilt block with a build (its
"s", upload included) longer than the margin is only reported. With --delete
it deletes the stale tiles.
Credentials in AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY, as for
own_tiles.py. Needs boto3.
"""
import argparse, json, os, re, sys

B = 4096  # block side, m (own_tiles.B)
TILE = 256  # samples per tile side
KEY = re.compile(r"^(?P<prefix>.+)/(?P<region>[^/]+)/(?P<product>ground|canopy)/(?P<s>\d+)/(?P<c>\d+)_(?P<r>\d+)\.hdt$")


def stale(objects, margin, builds):
    """objects: (key, last modified as a datetime); builds: (region, bx, by) ->
    the seconds each run took over that block. The stale keys, sorted; the
    blocks left alone for a build longer than the margin; the keys that are not
    tiles."""
    blocks, other = {}, []
    for key, t in objects:
        m = KEY.match(key)
        if not m:
            other.append(key)
            continue
        side = TILE * int(m["s"])
        b = (m["region"], int(m["c"]) * side // B, int(m["r"]) * side // B)
        blocks.setdefault(b, []).append((key, t))
    out, slow = [], []
    for b, tiles in blocks.items():
        runs = builds.get(b, [])
        if len(runs) < 2:
            continue
        newest = max(t for _, t in tiles)
        keys = [k for k, t in tiles if (newest - t).total_seconds() > margin]
        if keys and max(runs) > margin:
            slow.append((b, runs, len(keys)))
        else:
            out += keys
    return sorted(out), slow, other


def load_builds(runs):
    builds = {}
    for spec in runs:
        region, state = spec.split("=", 1)
        for line in open(os.path.join(os.path.expanduser(state), "done.jsonl")):
            j = json.loads(line)
            builds.setdefault((region, j["bx"], j["by"]), []).append(j["s"])
    return builds


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("runs", nargs="+", metavar="REGION=STATE_DIR")
    ap.add_argument("--bucket", required=True)
    ap.add_argument("--endpoint", required=True)
    ap.add_argument("--prefix", default="v1")
    ap.add_argument("--margin", type=float, default=3600)
    ap.add_argument("--delete", action="store_true", help="delete the stale tiles (otherwise only list them)")
    args = ap.parse_args()
    builds = load_builds(args.runs)
    import boto3

    s3 = boto3.client("s3", endpoint_url=args.endpoint)
    objects = []
    for page in s3.get_paginator("list_objects_v2").paginate(Bucket=args.bucket, Prefix=args.prefix + "/"):
        objects += [(o["Key"], o["LastModified"]) for o in page.get("Contents", [])]
        print("\rlisted %d" % len(objects), end="", file=sys.stderr, flush=True)
    print(file=sys.stderr)
    keys, slow, other = stale(objects, args.margin, builds)
    print("%d objects, %d not tiles, %d stale tiles" % (len(objects), len(other), len(keys)))
    for b, runs, n in slow:
        print("  left alone: block %s, builds of %s s, %d older tiles" % (b, runs, n))
    for k in other[:20]:
        print("  not a tile:", k)
    for k in keys:
        print(k)
    if args.delete:
        for i in range(0, len(keys), 1000):
            r = s3.delete_objects(Bucket=args.bucket, Delete={"Objects": [{"Key": k} for k in keys[i:i + 1000]]})
            for e in r.get("Errors", []):
                print("  error", e, file=sys.stderr)
        print("deleted %d" % len(keys))


if __name__ == "__main__":
    sys.exit(main())
