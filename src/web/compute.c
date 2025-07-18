/*
clang -O3 -msimd128 --no-standard-libraries --target=wasm32 compute.c -Wl,--no-entry -Wa,--no-type-check -o compute.wasm

WebAssembly.instantiateStreaming(fetch('compute.wasm'), {env: {memory: new WebAssembly.Memory({initial:32})}});
*/

#include <stdint.h>

 __attribute__((export_name("precompute")))
void precompute (int tile_width, int tile_height, double deltax, double deltay,
                 float tile[], float heights[], int8_t normals[]) {
  for (int y = 1; y <= tile_height - 2; y++) {
    for(int x = 1; x <= tile_width - 2; x++) {
      double nx =
        (tile[y * tile_width +  x - 1]
         - tile[y * tile_width + x + 1]) * deltay;
      double ny =
        (tile[(y - 1) * tile_width + x]
         - tile[(y + 1) * tile_width + x]) * deltax;
      double nz = 2 * deltax * deltay;
      double n = 127. / __builtin_sqrt (nx * nx + ny * ny + nz * nz);
      normals[(tile_height - 2 - y) * (tile_width - 2) * 3 + (x - 1) * 3] =
        nx * n;
      normals[(tile_height - 2 - y) * (tile_width - 2) * 3 + (x - 1) * 3 + 1] =
        ny * n;
      normals[(tile_height - 2 - y) * (tile_width - 2) * 3 + (x - 1) * 3 + 2] =
        nz * n;
      heights[(tile_height - 2 - y) * (tile_width - 2) + x - 1] =
        tile[y * tile_width + x];
    }
  }
}

 __attribute__((export_name("build_indices")))
void build_indices(int w, int h, uint32_t is []) {
  for (int i = 0; i <= h - 2; i++) {
    for (int j = 0; j <= w - 1; j++) {
      is[(i * (w + 1) * 2) + (j * 2) + 1] = j + (i * w);
      is[(i * (w + 1) * 2) + (j * 2)] = j + ((i + 1) * w);
    }
    if (i > 0) {
      is[(i * (w + 1) * 2) - 2] = (i - 1) * w + w - 1;
      is[(i * (w + 1) * 2) - 1] = (i + 1) * w;
    }
  }
}

void decode_fp (uint8_t in[], uint8_t out[], int w, int h) {
  for (int i = 0; i < h; i++) {
      for (int j = 0; j < w - 1; j++) {
        for (int k = 0; k < 4; k++) {
          out[i * w * 4 + 4 * j + k] =
            in[i * w * 4 + ((4 - k - 1) * w) + j];
        }
      }
  }
}

void decode_delta (uint8_t b[], int w, int h) {
  for (int i = 0; i < h; i++) {
    for (int j = 1; j < w; j++) {
      b[i * w + j] = b[i * w + j] + b[i * w + j - 1];
    }
  }
}

 __attribute__((export_name("decode_image")))
void decode_image (uint8_t in[], uint8_t out[], int w, int h) {
  decode_delta(in, w, h);
  decode_fp(in, out, w, h);
}
