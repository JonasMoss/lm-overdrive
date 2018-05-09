real distributions_lpdf(real[] Z, int N, real[] thetas, real[] SQRT_M,
                        real[] lower_bounds, real[] upper_bounds, int[] dist_indices, real[] p) {
  real lpdf = 0;

  // The likelihoods should correspond to those in .database$likelihood

  for(n in 1:N) {
    if(dist_indices[n] == 1) {
      // "Mixture, folded normal with lower bound",

      lpdf += mix_fnormal_lower_lpdf(Z[n] | SQRT_M[n]*thetas[n], lower_bounds[n], p[n]);

    } else if(dist_indices[n] == 2) {
      // "Mixture, folded normal with upper bound",

      lpdf += mix_fnormal_upper_lpdf(Z[n] | SQRT_M[n]*thetas[n], upper_bounds[n], p[n]);

    } else if(dist_indices[n] == 3) {
      // "Mixture, folded normal with double bounds",

      lpdf += mix_fnormal_double_lpdf(Z[n] | SQRT_M[n]*thetas[n], lower_bounds[n], upper_bounds[n], p[n]);

    } else if(dist_indices[n] == 4) {
      // "Mixture, folded normal with inner bounds",

      lpdf += mix_fnormal_inner_lpdf(Z[n] | SQRT_M[n]*thetas[n], lower_bounds[n], upper_bounds[n], p[n]);

    } else if(dist_indices[n] == 5) {
      // "Mixture, normal with lower bound",

      lpdf += mix_normal_lower_lpdf(Z[n] | SQRT_M[n]*thetas[n], lower_bounds[n], p[n]);

    } else if(dist_indices[n] == 6) {
      // "Mixture, normal with upper bound",

      lpdf += mix_normal_upper_lpdf(Z[n] | SQRT_M[n]*thetas[n], upper_bounds[n], p[n]);

    } else if(dist_indices[n] == 7) {
      // "Mixture, normal with double bounds",

      lpdf += mix_normal_double_lpdf(Z[n] | SQRT_M[n]*thetas[n], lower_bounds[n], upper_bounds[n], p[n]);

    } else if(dist_indices[n] == 8) {
      // "Mixture, normal with double bounds",

      lpdf += mix_normal_inner_lpdf(Z[n] | SQRT_M[n]*thetas[n], lower_bounds[n], upper_bounds[n], p[n]);

    } else if(dist_indices[n] == 9) {
      // "Folded normal, affects p"

      lpdf += fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 10) {
      // "Normal, affects p"

      lpdf += fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 11) {
      // "Lower truncated folded normal, affects p",

      lpdf += lower_fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n]);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 12) {
      //  "Upper truncated folded normal, affects p."

      lpdf += upper_fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, upper_bounds[n]);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 13) {
      //  "Doubly truncated folded normal, affects p."

      lpdf += double_fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n], upper_bounds[n]);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 14) {
      //  "Inner truncated folded normal, affects p."

      lpdf += inner_fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n], upper_bounds[n]);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 15) {
      // "Lower truncated folded normal, affects p",

      lpdf += lower_normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n]);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 16) {
      //  "Upper truncated normal, affects p."

      lpdf += upper_normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, upper_bounds[n]);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 17) {
      //  "Doubly truncated normal, affects p."

      lpdf += double_normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n], upper_bounds[n]);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 18) {
      //  "Inner truncated normal, affects p."

      lpdf += inner_normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n], upper_bounds[n]);
      lpdf += bernoulli_lpmf(0 | p[n]);

    } else if(dist_indices[n] == 19) {
      // "Folded normal, does not affect p"

      lpdf += fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1);

    } else if(dist_indices[n] == 20) {
      // "Normal, does not affect p"

      lpdf += fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1);

    } else if(dist_indices[n] == 21) {
      // "Lower truncated folded normal, does not affect p",

      lpdf += lower_fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n]);

    } else if(dist_indices[n] == 22) {
      //  "Upper truncated folded normal, does not affect p."

      lpdf += upper_fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, upper_bounds[n]);

    } else if(dist_indices[n] == 23) {
      //  "Doubly truncated folded normal, does not affect p."

      lpdf += double_fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n], upper_bounds[n]);

    } else if(dist_indices[n] == 24) {
      //  "Inner truncated folded normal, does not affect p."

      lpdf += inner_fnormal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n], upper_bounds[n]);

    } else if(dist_indices[n] == 25) {
      // "Lower truncated folded normal, does not affect p",

      lpdf += lower_normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n]);

    } else if(dist_indices[n] == 26) {
      //  "Upper truncated normal, does not affect p."

      lpdf += upper_normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, upper_bounds[n]);

    } else if(dist_indices[n] == 27) {
      //  "Doubly truncated normal, does not affect p."

      lpdf += double_normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n], upper_bounds[n]);

    } else if(dist_indices[n] == 28) {
      //  "Inner truncated normal, does not affect p."

      lpdf += inner_normal_lpdf(Z[n] | SQRT_M[n]*thetas[n], 1, lower_bounds[n], upper_bounds[n]);

    }

  }

  return lpdf;

}
