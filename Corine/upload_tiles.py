"""Upload a directory of tiles to an S3-compatible bucket (Cloudflare R2).

    upload_tiles.py <dir> <prefix> --bucket B --endpoint URL [--only SUBDIR...]

Every file under <dir> goes to <prefix>/<its path under dir>, with the headers
of own_tiles.py's: immutable for a year, the client's data cache keeping it for
ever anyway. A tile set that changes moves to a new prefix (v1, v2, ...).
Credentials come from the usual AWS environment variables. Needs boto3.

    upload_tiles.py ~/mountains-tiles/v2 v2 --only dem clc clc4 poi --bucket B --endpoint E
"""

import argparse
import os
import sys
from concurrent.futures import ThreadPoolExecutor


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("dir")
    ap.add_argument("prefix")
    ap.add_argument("--bucket", required=True)
    ap.add_argument("--endpoint", required=True)
    ap.add_argument("--only", nargs="+", help="these subdirectories of dir only")
    ap.add_argument("--workers", type=int, default=16)
    args = ap.parse_args()

    import boto3

    s3 = boto3.client("s3", endpoint_url=args.endpoint)
    roots = [os.path.join(args.dir, d) for d in args.only] if args.only else [args.dir]
    files = sorted(
        os.path.join(d, f)
        for root in roots
        for d, _, fs in os.walk(root)
        for f in fs
        if not f.endswith(".tmp")
    )
    keys = [args.prefix + "/" + os.path.relpath(p, args.dir).replace(os.sep, "/") for p in files]

    def put(path, key):
        with open(path, "rb") as f:
            s3.put_object(
                Bucket=args.bucket,
                Key=key,
                Body=f.read(),
                ContentType="application/octet-stream",
                CacheControl="public, max-age=31536000, immutable",
            )

    done = 0
    with ThreadPoolExecutor(args.workers) as pool:
        for _ in pool.map(put, files, keys):
            done += 1
            if done % 500 == 0:
                print(f"{done}/{len(files)}", file=sys.stderr, flush=True)
    print(f"{len(files)} files under {args.prefix}/", file=sys.stderr)


if __name__ == "__main__":
    main()
