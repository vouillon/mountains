#!/bin/bash
# Batch generate CLC tiles for the Alps region
# Approximate bounds: Lat 44-48°N, Lon 5-17°E

DB="Corine/CLC2018_CLC2018_V2018_20.gpkg"
OUT_DIR="data/clc"

# Build the extractor first
dune build Corine/extract_tiles.exe

# Generate tiles
for lat in $(seq 43 47); do
    for lon in $(seq 5 9); do
        # Format tile name: N45E006
        tile=$(printf "N%02dE%03d" $lat $lon)
        echo "Processing $tile..."
        dune exec -- ./Corine/extract_tiles.exe "$DB" "$OUT_DIR" "$tile" 2>&1 | tail -1
    done
done

echo "Done! Generated tiles:"
ls -la "$OUT_DIR"/*.clc | wc -l
