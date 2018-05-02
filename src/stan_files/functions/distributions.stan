real distributions_lpdf(real[] Z, int N, real[] thetas, real[] SQRT_M,
                        real[] lower_bounds, real[] upper_bounds, int[] dist_indices, real[] p) {
  real lpdf = 0;
  for(n in 1:N) {
    if(dist_indices[n] == 1) {
      // Distribution 1: Mixture, folded normal. Both possible.

      lpdf += mix_fnormal_lower_lpdf(Z[n] | SQRT_M[n]*thetas[n], lower_bounds[n], p[n]);

    } else if(dist_indices[n] == 2) {
      // Distribution 2: Mixture, normal with upper bound. Both possible.

      lpdf += mix_normal_upper_lpdf(Z[n] | SQRT_M[n]*thetas[n], upper_bounds[n], p[n]);

    } else if(dist_indices[n] == 3) {
      // Distribution 3: Mixture, normal with lower bound. Both possible.

      lpdf += mix_normal_lower_lpdf(Z[n] | SQRT_M[n]*thetas[n], lower_bounds[n], p[n]);

    } else if(dist_indices[n] == 4) {
      // Distribution 4: Known fnormal. Not truncated. Does not affect p[n].

      lpdf += fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1);

    } else if(dist_indices[n] == 5) {
      // Distribution 5: Known fnormal. Not truncated. Does affect p[n].

      lpdf += fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 6) {
      // Distribution 6: Known normal. Not truncated. Does not affect p[n].

      lpdf += normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1);

    } else if(dist_indices[n] == 7) {
      // Distribution 7: Known normal. Not truncated. Does affect p[n].

      lpdf += normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 8) {
      // Lower truncated folded normal, does not affect p.

      lpdf += lower_fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n]);

    } else if(dist_indices[n] == 9) {
      // Lower truncated folded normal, affects p.

      lpdf += lower_fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n]);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 10) {
      // Lower truncated normal, does not affect p"

      lpdf += lower_normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n]);

    } else if(dist_indices[n] == 11) {
      // "Lower truncated normal, affects p."

      lpdf += lower_normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n]);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 12) {
      // "Upper truncated normal, does not affect p",

      lpdf += upper_normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, upper_bounds[n]);

    }else if(dist_indices[n] == 13) {
      //  "Upper truncated normal, affects p."

      lpdf += upper_normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, upper_bounds[n]);
      lpdf += bernoulli_lpmf(0 | p[n]);

    }
  }

  return lpdf;

}
