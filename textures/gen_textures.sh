#!/bin/sh
set -e
cd "$(dirname "$0")"

magick convert rock.png grass.png forest.png ice.png -resize 1024x1024 -combine details.png
toktx --assign_oetf linear --encode uastc --genmipmap details.ktx2 details.png
ktx transcode --target bc7 details.ktx2 ../assets/details_bc7.ktx2
ktx transcode --target astc details.ktx2 ../assets/details_astc.ktx2
ktx transcode --target etc-rgba details.ktx2 ../assets/details_etc2.ktx2
