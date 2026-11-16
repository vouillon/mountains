MISC
====
- Speed-up visibility?

RENDERING
=========
- Sea from OSM as well (inland water is: CLC 511/512 are dropped and filled
  with the land around on the GPU, cover_fill.frag, under the OSM water)
  => OSM water polygons (osmdata.openstreetmap.de) in the tiles' water layer
  => OSM water in the far field (data/clc4), which still has CLC water
- Water reflection
  => ray-marching using a min-max height pyramid
  => multi-plane baked cubemaps
- roads, paths, trails
  => RGB SDF clipmap
     ~ 1m/texel / 2048x2048 at the highest resolution

DATA PROCESSING
===============
- evaluate triangulation algorithm using benchs and tests from
  https://github.com/mapbox/earcut
