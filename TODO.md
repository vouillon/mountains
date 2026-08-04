MISC
====
- Speed-up visibility?

RENDERING
=========
- CLC water data bleeding (over large / missing islands)
  => remove CLC water / jump flood on the GPU before drawing the OSM water
  => or fix the features in the tiles?
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
