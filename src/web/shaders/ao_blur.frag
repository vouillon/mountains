
uniform sampler2D ao_tex;
uniform sampler2D relief;
uniform vec2 inv_res;
in vec2 uv;
out float color;

void main() {
  float result = 0.0;
  float weight_sum = 0.0;

  // Center height for bilateral comparison
  float h_center = decode_height(texture(relief, uv).rg);

  // Bilateral blur: Gaussian spatial + height similarity
  // Height sigma: ~50m (samples with >50m difference get low weight)
  float h_sigma = 50.0;
  float h_sigma_sq2 = 2.0 * h_sigma * h_sigma;

  // 3x3 Gaussian spatial weights
  float k[9];
  k[0] = 1.;
  k[1] = 2.;
  k[2] = 1.;
  k[3] = 2.;
  k[4] = 4.;
  k[5] = 2.;
  k[6] = 1.;
  k[7] = 2.;
  k[8] = 1.;

  int idx = 0;
  for (int y = -1; y <= 1; y++) {
    for (int x = -1; x <= 1; x++) {
      vec2 sample_uv = uv + vec2(float(x), float(y)) * inv_res;
      float ao_sample = texture(ao_tex, sample_uv).r;
      float h_sample = decode_height(texture(relief, sample_uv).rg);

      // Height difference weight (bilateral term)
      float h_diff = h_sample - h_center;
      float h_weight = exp(-(h_diff * h_diff) / h_sigma_sq2);

      // Combined weight: spatial * height similarity
      float w = k[idx] * h_weight;
      result += ao_sample * w;
      weight_sum += w;
      idx++;
    }
  }
  color = result / weight_sum;
}
