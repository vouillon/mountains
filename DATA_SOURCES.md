# Data sources

Where every piece of data the app shows comes from, under which terms, and how
it was turned into what the client reads. The tools' own comments hold the
details; this is the map to them.

| Data | Source | Licence | Where the client reads it | Made by |
| --- | --- | --- | --- | --- |
| Base elevation, 1" | Copernicus GLO-30 DSM | Copernicus DEM licence | `data/dem` (DEM1) | `Corine/compress_dem.ml` |
| Far-field elevation, 4" | Copernicus GLO-30 DSM | same | `data/dem4` (DEM1) | `compress_dem.exe --coarse` |
| Land cover | CORINE Land Cover 2018 | Copernicus Land Monitoring Service | `data/clc` (CLC6) | `Corine/extract_tiles.ml` |
| Far-field land cover | CLC 2018 + OSM water | both | `data/clc4` | `extract_tiles.exe --coarse` |
| Water, sea, summits | OpenStreetMap, via Overture Maps | ODbL | inside the CLC6 tiles, `data/poi` | `Corine/fetch_overture.sh`, `overture.ml` |
| Tree cover and leaf type | EEA HRL Tree Cover Density / Dominant Leaf Type 2018 | Copernicus Land Monitoring Service | fetched at run time | `src/web/tree_cover.ml` |
| High-resolution ground and canopy | IGN LiDAR HD, swisstopo, Regione Piemonte, TINITALY, Regione Valle d'Aosta | Etalab 2.0, swisstopo open data, CC BY 4.0 | `tiles.vouillon.org/v1` (HDT1) | `Corine/own_tiles.py` |
| 2-D map picker | IGN Plan IGN v2, LiDAR HD shading, contours | Etalab 2.0 | fetched at run time | `src/web/map_picker.ml` |

The credits the app shows are in `src/web/index.html` (the startup notice) and
`src/web/map_picker.ml` (the map's attribution); keep them in step with this
file.

## Base elevation: `data/dem`

- **Source**: Copernicus GLO-30 DSM, 1 arc-second, as cloud-optimised GeoTIFFs
  from the public bucket
  `https://copernicus-dem-30m.s3.amazonaws.com/Copernicus_DSM_COG_10_<N45>_00_<E006>_00_DEM/…tif`.
  It is a surface model (it includes forests and buildings), which the near
  rings replace with ground.
- **Credit**: "Produced using Copernicus WorldDEM-30 © DLR e.V. 2010-2014 and ©
  Airbus Defence and Space GmbH 2014-2018 provided under COPERNICUS by the
  European Union and ESA; all rights reserved."
- **Import**: `Corine/fetch_dem.sh <lat> <lon>` downloads one 1° cell into
  `data/` (the .tif files, ~40 MB each, are not tracked), then
  `dune exec Corine/compress_dem.exe -- data/<file>.tif data` writes its nine
  1200 × 1200 sub-tiles `data/dem/N<lat>_E<lon>_<row>_<col>.dem` (row 0 south).
  `compress_dem.exe <lat> <lon> data` does both and deletes the .tif. A cell the
  bucket lacks (open sea) is written as zeros.
- **Format**: DEM1, heights as u16 over -500…9000 m (14.5 cm steps),
  parallelogram-predicted, zigzagged, split into high and low bytes, each
  zlib-compressed. The range is hard-coded in the client too: changing it means
  regenerating every tile.
- **Coverage**: 32 cells (288 files): N42 E008-009, N43-N46 E004-009, and La
  Réunion (S21-S22 E054-056).

## Far-field elevation: `data/dem4`

The same GLO-30 cells at 4 arc-seconds: `compress_dem.exe --coarse <lat> <lon>
data`, nine 300 × 300 sub-tiles per cell in `data/dem4`. Each sample is a soft
maximum of its 4 × 4 block (k = 0.25), the same as the client's own downsample
(`downsample.frag`, `Viewer.downsample_k`), so distant summits keep their
height. 50 cells (450 files), N41-N47 × E003-E010 without sea, and La Réunion.

## Land cover: `data/clc`

- **Source**: CORINE Land Cover 2018 (v2018_20), as the GeoPackage
  `Corine/CLC2018_CLC2018_V2018_20.gpkg` (not tracked; downloadable from
  `https://data.cquest.org/corine_land_cover/eu/`). Table
  `CLC2018_CLC2018_V2018_20` (EPSG:3035) for Europe,
  `CLC2018_CLC2018_V2018_20_FR_REU` (EPSG:2975) for La Réunion.
- **Credit**: "Generated using European Union's Copernicus Land Monitoring
  Service information."
- **Import**: `extract_tiles.exe --overture <dir> <gpkg> data/clc N45E006`
  clips the polygons to the cell plus 0.005°, repairs and triangulates them,
  adds the water and summits of `<dir>` (see below), and writes CLC6. Without
  `--overture` it asks the Overpass API instead (the older path).
  `Corine/batch_extract_alps.sh` is a stale example: it runs without
  `--overture` over N43-47 E005-009.
- **Format**: CLC6 (`Corine/clc_file.ml`): triangles in 16-bit coordinates,
  water in 24-bit ones (~40 cm), and points of interest. `extract_tiles.exe
  --convert` turned the earlier CLC5 tiles into CLC6.
- **Coverage**: 65 cells, N42-N47 from E004 to E016, and La Réunion. All were
  regenerated from Overture's 2026-10-21.0 release.

## Far-field land cover: `data/clc4`

`extract_tiles.exe --coarse --overture <dir> <gpkg> data/clc4 <tile>`: a grid of
palette indices at 4", in the DEM1 framing (nine 300 × 300 sub-tiles, the
elevation range in the header meaningless). OSM water first (a sample is water
when at least half of its 16 arc-seconds are), patches under 8 samples dropped,
then CORINE, holes filled with the nearest land, the rest sea. 53 cells.

## Water, sea and summits: OpenStreetMap via Overture

- **Source**: Overture Maps' base theme, which carries OpenStreetMap's water
  and land features. Release 2026-10-21.0 for the current tiles (the script does
  not pin one).
- **Credit**: "© OpenStreetMap contributors, Overture Maps Foundation (Open
  Database License)."
- **Import**: `Corine/fetch_overture.sh <dir> <tile>...` (needs `pip install
  overturemaps`) downloads each cell ± 0.01°: water polygons (no swimming
  pools) into `<tile>.water.geojsonl`, `peak` and `saddle` points into
  `<tile>.pois.geojsonl`. `Corine/overture.ml` maps water to CORINE classes
  (ocean 523, lagoon 521, rivers and canals 511, the rest 512) and names summits
  in French where Overture has a French name.
- **`data/poi`**: POI-only CLC6 tiles, `extract_tiles.exe --pois
  data/clc/<t>.clc data/poi/<t>.clc`, one per cell of `data/clc` and
  `data/clc4` (81).
- `data/points.geojson` is an older overpass-turbo export of summits
  (2024-08-29) read only by the SDL client.

## Tree cover, at run time

`src/web/tree_cover.ml` asks the EEA's image server
(`image.discomap.eea.europa.eu/…/GioLandPublic/<layer>/ImageServer/exportImage`)
for `HRL_TreeCoverDensity_2018` (bilinear) and `HRL_DominantLeafType2018`
(nearest), 10 m layers, in blocks of 1024 samples at ~20 m or ~80 m. The
service makes its samples square in degrees, so the request's width is fitted to
the box. The service worker caches the answers. Near the eye, where the
high-resolution canopy has data, trees come from the canopy instead.

## High-resolution ground and canopy: our own tiles

The near rings (ground at 1, 4 and 8 m; canopy at 2, 4 and 16 m) come from
`https://tiles.vouillon.org/v1`: HDT1 tiles we built and host on Cloudflare R2
(`own_tiles_url` in `src/web/hd_dem.ml`; `?ign` switches back to IGN's
services, `?own=<url>` to another copy). Format, layout and every choice are in
the docstring of `Corine/own_tiles.py`. A missing tile is all nodata, and the
client then fades to the base elevation.

### Sources

| Where | Product | Resolution, date | Heights | Access | Credit |
| --- | --- | --- | --- | --- | --- |
| France, La Réunion | IGN LiDAR HD MNT (ground) and MNH (canopy height) | 1 m (canopy 2 m), 2020s | NGF-IGN69 | WMS `data.geopf.fr/wms-r`, BIL32, layers `IGNF_LIDAR-HD_MN[TH]_ELEVATION.ELEVATIONGRIDCOVERAGE.<LAMB93\|RGR92UTM40S>` | IGN, licence Etalab 2.0 |
| Switzerland | swisstopo swissALTI3D (ground) and swissSURFACE3D Raster (surface) | 0.5 m, read at 1 m | LN02 | STAC API `data.geo.admin.ch/api/stac/v0.9`, 1 km COGs | © swisstopo |
| Piedmont | Regione Piemonte DTM ICE 2009-2011 | 5 m | — | WCS 1.0.0 (`geomap.reteunitaria.piemonte.it/…/wcs_ice_2009_2011_dtm`) | Regione Piemonte, CC BY 4.0 |
| Italy elsewhere | INGV TINITALY 1.1 | 10 m | — | 50 km zips from `tinitaly.pi.ingv.it` | Tarquini S. et al. (2023), doi:10.13127/tinitaly/1.1, CC BY 4.0 |
| Aosta Valley | Regione Valle d'Aosta DTM 2005/2008 | 2 m LIDAR, 2005 and 2008 | orthometric | one zip, `geoprodotti.regione.vda.it/download/DTM0508_002/DTM0508_002_UNICO.zip` | "Dati estratti dal Modello Digitale del Terreno (DTM) della Regione Autonoma Valle d'Aosta", CC BY 4.0 |

### How they are combined

Each source is used where it alone has data. Where two overlap, the better one
fades into the other over the strip next to the ground only the other covers,
so a height difference becomes a gentle slope rather than a step. Priority:
IGN, then swisstopo, then the Aosta Valley, then Piemonte, then TINITALY.

| Join | Measured difference | Fade |
| --- | --- | --- |
| IGN → swisstopo | swisstopo 0.25-0.5 m lower | 200 m |
| IGN, swisstopo → Aosta | Aosta 0.25 m higher (corrected); 3-6 m on glaciers, 15 years older | 500 m |
| IGN, swisstopo → Piemonte, TINITALY | Piemonte within 0.2 m; TINITALY ±0.6 m, 12 m on the Mont Blanc glaciers | 500 m |
| Piemonte → TINITALY | — | 200 m |

Ground at 1 m is written where IGN, swisstopo or the Aosta DTM has data. The
coarser Italian models only give the 4 m and 8 m levels, and canopy comes from
IGN and swisstopo alone (elsewhere the client uses the EEA tree cover).

### The runs

Each run has a state directory under `~/mountains-tiles` (block list,
`done.jsonl` of the finished blocks; a run resumes where it stopped). A later
run rewrites the blocks it shares with an earlier one, so the order matters.
Uploads use the bucket owner's R2 keys, set in the environment.

| Order | Command (`Corine/own_tiles.py …`) | What | Blocks | Tiles |
| --- | --- | --- | --- | --- |
| 1 | `state-lamb93 lamb93` | IGN, metropolitan France | 5153 | 1,652,941 |
| 2 | `state-reunion rgr92utm40s` | IGN, La Réunion | 257 | 57,669 |
| 3 | `state-swiss lamb93 --swiss` | + swisstopo | 871 | 281,253 |
| 4 | `state-lamb93-edges lamb93` | IGN edge repair (below) | 894 | 283,756 |
| 5 | `state-reunion-edges rgr92utm40s` | same, La Réunion | 112 | 30,179 |
| 6 | `state-swiss-edges lamb93 --swiss` | same, Swiss blocks | 130 | 46,370 |
| 7 | `state-italy lamb93 --italy` | + Piemonte, TINITALY | 3123 | 198,184 |
| 8 | `state-aosta lamb93 --italy --aosta` | + the Aosta Valley | 248 | — |

Every run takes `--bucket <bucket> --endpoint <R2 endpoint>` (or `--out <dir>`
to build locally) and `--jobs N`. After a run that rebuilds blocks:

1. `Corine/own_tiles_stale.py lamb93=<state dir> … --bucket … --endpoint …`
   lists the tiles a rebuild left behind (no run deletes anything; a rebuilt
   block that came out with fewer tiles keeps its old ones). Check the list,
   then run it again with `--delete`.
2. Purge Cloudflare's cache: tiles are served with a one-year immutable
   `Cache-Control`.
3. Clear the app's site data in the browsers that used the old tiles: the
   service worker keeps tiles, and their absence, for good.

The edge repair runs (4-6) were planned by `Corine/own_tiles_edges.py`: the
published blocks with nodata in their 8 m ground or 16 m canopy, and their
neighbours.

### Traps met on the way

- **IGN's WMS mixes its nodata into its data.** It resamples with -9999 as a
  height, so the samples next to any edge of its data (borders, coasts, holes)
  are mixtures (766 m, -16 m on a 2540 m slope). `wms` drops every sample within
  two of its nodata. The first runs had them; runs 4-6 rebuilt those blocks.
- **IGN's WMS is rate-limited**: 40 requests/s, then HTTP 400 for 5 s. The runs
  retry.
- **swisstopo's COG overviews are nearest-neighbour**, not means: the first
  overview is every other sample, a quarter metre north-west of where the file
  says. Read shifted back, it is within 2.3 cm of the 0.5 m mean, for a quarter
  of the download. GDAL's Warp silently reads overviews unless told
  `overviewLevel="NONE"`. A few border scraps have no overview or no exact half
  and are read whole.
- **swisstopo canopy over glaciers**: the latest ground model is often newer
  than the surface model; surface minus that ground reads thinned glaciers as
  trees. Canopy uses the ground of the surface's year.
- **Piemonte's WCS** only works in version 1.0.0 (2.0.1 subsets fail), and
  answers an exception report, not an empty image, outside its bounding box.
- **The Aosta zip** is one 9.9 GB ASCII grid with decimal commas, compressed
  with Deflate64 (Info-ZIP's `unzip` reads it, Python's `zipfile` does not); the
  server fails range requests beyond 2 GB. `own_tiles.aosta` streams it into a
  1.1 GB GeoTIFF of centimetres. Its ED50 coordinates, as GDAL transforms them,
  are 0.5 m west and 2 m north of IGN's and swisstopo's (measured at both
  Saint-Bernard passes, 0.21 m RMS once moved): `AOSTA_SHIFT`.
- **Uploads can stall for hours** (a laptop asleep): one block's tiles went up
  15 hours apart, which is why `own_tiles_stale.py` only looks at blocks
  several runs built.
- **The public URL needs a User-Agent**: Cloudflare answers 403 to Python's
  default one. A query string (`?v=x`) bypasses the CDN cache, for checks.

## IGN services at run time

With `?ign` the 3-D rings read IGN's services directly, as before the own tiles:
WMTS `ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES` (RGE ALTI, level 13) and the
LiDAR HD WMS layers above. The 2-D map picker always uses IGN's WMTS:
`GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2`,
`IGNF_LIDAR-HD_MNT_ELEVATION.ELEVATIONGRIDCOVERAGE.SHADOW` and
`ELEVATION.CONTOUR.LINE`, credited "© IGN — Plan IGN, Relief du terrain HD,
Courbes de niveau".
