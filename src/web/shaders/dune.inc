(rule
 (target ao_bake.frag.fmt)
 (deps ao_bake.frag)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff ao_bake.frag ao_bake.frag.fmt)))

(rule
 (target ao_blur.frag.fmt)
 (deps ao_blur.frag)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff ao_blur.frag ao_blur.frag.fmt)))

(rule
 (target clc_raster.frag.fmt)
 (deps clc_raster.frag)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff clc_raster.frag clc_raster.frag.fmt)))

(rule
 (target clc_raster.vert.fmt)
 (deps clc_raster.vert)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff clc_raster.vert clc_raster.vert.fmt)))

(rule
 (target common_header.frag.fmt)
 (deps common_header.frag)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff common_header.frag common_header.frag.fmt)))

(rule
 (target downsample.frag.fmt)
 (deps downsample.frag)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff downsample.frag downsample.frag.fmt)))

(rule
 (target normal.frag.fmt)
 (deps normal.frag)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff normal.frag normal.frag.fmt)))

(rule
 (target quad.vert.fmt)
 (deps quad.vert)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff quad.vert quad.vert.fmt)))

(rule
 (target radial_common.vert.fmt)
 (deps radial_common.vert)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff radial_common.vert radial_common.vert.fmt)))

(rule
 (target shadow.frag.fmt)
 (deps shadow.frag)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff shadow.frag shadow.frag.fmt)))

(rule
 (target shadow_main.vert.fmt)
 (deps shadow_main.vert)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff shadow_main.vert shadow_main.vert.fmt)))

(rule
 (target shadow_prefix.vert.fmt)
 (deps shadow_prefix.vert)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff shadow_prefix.vert shadow_prefix.vert.fmt)))

(rule
 (target sky.frag.fmt)
 (deps sky.frag)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff sky.frag sky.frag.fmt)))

(rule
 (target sky.vert.fmt)
 (deps sky.vert)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff sky.vert sky.vert.fmt)))

(rule
 (target terrain.frag.fmt)
 (deps terrain.frag)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff terrain.frag terrain.frag.fmt)))

(rule
 (target terrain_main.vert.fmt)
 (deps terrain_main.vert)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff terrain_main.vert terrain_main.vert.fmt)))

(rule
 (target terrain_prefix.vert.fmt)
 (deps terrain_prefix.vert)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff terrain_prefix.vert terrain_prefix.vert.fmt)))

(rule
 (target text.frag.fmt)
 (deps text.frag)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff text.frag text.frag.fmt)))

(rule
 (target text.vert.fmt)
 (deps text.vert)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff text.vert text.vert.fmt)))

(rule
 (target triangle.frag.fmt)
 (deps triangle.frag)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff triangle.frag triangle.frag.fmt)))

(rule
 (target triangle.vert.fmt)
 (deps triangle.vert)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff triangle.vert triangle.vert.fmt)))

(rule
 (target water_raster.frag.fmt)
 (deps water_raster.frag)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff water_raster.frag water_raster.frag.fmt)))

(rule
 (target water_raster.vert.fmt)
 (deps water_raster.vert)
 (action
  (with-stdout-to %{target}
   (run clang-format %{deps}))))

(rule
 (alias fmt)
 (action
  (diff water_raster.vert water_raster.vert.fmt)))

