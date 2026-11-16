#!/bin/bash
# Fetch what extract_tiles --overture reads of Overture Maps (OSM's data, by
# bounding box), two GeoJSON-sequence files per tile:
#   <tile>.water.geojsonl  the water areas, with the sea as polygons: no
#                          streams as lines, no springs, and no swimming
#                          pools, which are nine tenths of a coastal cell;
#   <tile>.pois.geojsonl   the peaks and the saddles.
#
#   fetch_overture.sh <output dir> <tile>...     (e.g. N45E006 S21E055)
#
# Needs the overturemaps command (pip install overturemaps), or its path in
# OVERTUREMAPS.

set -eu
out="$1"
shift
mkdir -p "$out"
fetch() { # type, tile, bbox
    "${OVERTUREMAPS:-overturemaps}" download --bbox="$3" -f geojsonseq \
        -t "$1" -o "$out/$2.all"
}
for tile in "$@"; do
    lat=$((10#${tile:1:2}))
    lon=$((10#${tile:4:3}))
    [ "${tile:0:1}" = S ] && lat=$((-lat))
    [ "${tile:3:1}" = W ] && lon=$((-lon))
    # The tile, and more than the margin extract_tiles cuts it with
    bbox=$(awk -v a="$lat" -v o="$lon" \
        'BEGIN { printf "%.2f,%.2f,%.2f,%.2f", o - 0.01, a - 0.01, o + 1.01, a + 1.01 }')
    echo "Fetching $tile ($bbox)..."
    fetch water "$tile" "$bbox"
    grep -E '"type": ?"(Multi)?Polygon"' "$out/$tile.all" |
        grep -v -E '"class": ?"swimming_pool"' >"$out/$tile.tmp" || true
    mv "$out/$tile.tmp" "$out/$tile.water.geojsonl"
    fetch land "$tile" "$bbox"
    grep -E '"class": ?"(peak|saddle)"' "$out/$tile.all" >"$out/$tile.tmp" || true
    mv "$out/$tile.tmp" "$out/$tile.pois.geojsonl"
    rm -f "$out/$tile.all" "$out/$tile.all.state"
done
