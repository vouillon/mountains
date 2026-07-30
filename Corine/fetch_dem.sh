#!/bin/sh
# Fetch a Copernicus GLO-30 source tile into data/ for Corine/compress_dem.exe.
# The .tif sources are not tracked (see .gitignore): they weigh ~40 MB per
# 1x1 degree cell and are re-downloadable from the public Copernicus bucket.
#
# Usage: Corine/fetch_dem.sh <lat> <lon>
#   e.g. Corine/fetch_dem.sh 44 5      (cell N44 E005)
#        Corine/fetch_dem.sh -21 55    (cell S21 E055)
set -e
lat=$1
lon=$2
if [ "$lat" -lt 0 ]; then latc=$(printf 'S%02d' $((-lat))); else latc=$(printf 'N%02d' "$lat"); fi
if [ "$lon" -lt 0 ]; then lonc=$(printf 'W%03d' $((-lon))); else lonc=$(printf 'E%03d' "$lon"); fi
f="Copernicus_DSM_COG_10_${latc}_00_${lonc}_00_DEM"
curl -f -o "data/$f.tif" "https://copernicus-dem-30m.s3.amazonaws.com/$f/$f.tif"
echo "data/$f.tif"
