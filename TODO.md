MISC
====
- Adjust contrast

RENDERING
=========
- High precision elevation at short distances
- CLC water data bleeding (over large / missing islands)
  => remove CLC water / jump flood on the GPU before drawing the OSM water
  => or fix the features in the tiles?
- Render a path on the terrain
  => turn the path into a ribbon, draw it ~15cm above the ground
  => constant screen pixels (screen space expansion in the vertex shader)
- Water reflection
  => ray-marching using a min-max height pyramid
  => multi-plane baked cubemaps

DATA PROCESSING
===============
- evaluate triangulation algorithm using benchs and tests from
  https://github.com/mapbox/earcut
