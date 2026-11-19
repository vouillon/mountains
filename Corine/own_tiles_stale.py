#!/usr/bin/env python3
"""The tiles a rebuild of their block left behind in the bucket.

    own_tiles_stale.py --bucket B --endpoint E [--prefix v1] [--delete]

A run of own_tiles.py uploads the tiles a block has data in, overwriting those
already there; it deletes nothing. A block rebuilt with less data (the edge
repairs, once wms dropped IGN's mixtures at its nodata, emptied a few tiles
along borders and coasts) keeps its old tiles where the new build has none:
there they still show the old trenches. This lists the bucket and, block by
block (4096 m, per region), reports the tiles uploaded more than --margin
seconds before the block's newest one: a block's tiles all go up within
minutes, and every run rebuilds whole blocks. With --delete it deletes them.
Credentials in AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY, as for
own_tiles.py. Needs boto3.
"""
import argparse, re, sys

B = 4096  # block side, m (own_tiles.B)
TILE = 256  # samples per tile side
KEY = re.compile(r"^(?P<prefix>.+)/(?P<region>[^/]+)/(?P<product>ground|canopy)/(?P<s>\d+)/(?P<c>\d+)_(?P<r>\d+)\.hdt$")


def stale(objects, margin):
    """objects: (key, last modified as a datetime). The stale keys, sorted, and
    the keys that are not tiles."""
    blocks, other = {}, []
    for key, t in objects:
        m = KEY.match(key)
        if not m:
            other.append(key)
            continue
        side = TILE * int(m["s"])
        b = (m["prefix"], m["region"], int(m["c"]) * side // B, int(m["r"]) * side // B)
        blocks.setdefault(b, []).append((key, t))
    out = []
    for tiles in blocks.values():
        newest = max(t for _, t in tiles)
        out += [k for k, t in tiles if (newest - t).total_seconds() > margin]
    return sorted(out), other


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--bucket", required=True)
    ap.add_argument("--endpoint", required=True)
    ap.add_argument("--prefix", default="v1")
    ap.add_argument("--margin", type=float, default=3600)
    ap.add_argument("--delete", action="store_true", help="delete the stale tiles (otherwise only list them)")
    args = ap.parse_args()
    import boto3

    s3 = boto3.client("s3", endpoint_url=args.endpoint)
    objects = []
    for page in s3.get_paginator("list_objects_v2").paginate(Bucket=args.bucket, Prefix=args.prefix + "/"):
        objects += [(o["Key"], o["LastModified"]) for o in page.get("Contents", [])]
        print("\rlisted %d" % len(objects), end="", file=sys.stderr, flush=True)
    print(file=sys.stderr)
    keys, other = stale(objects, args.margin)
    print("%d objects, %d not tiles, %d stale tiles" % (len(objects), len(other), len(keys)))
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
