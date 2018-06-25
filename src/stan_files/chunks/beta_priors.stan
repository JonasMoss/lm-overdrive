real unbounded_lpdf(real[] beta , int Q, int[] no, int[,] prior_types, real[,,] prior) {

  int index = 1;
  real lpdf = 0 ;

  for(q in 1:Q) {
    if(no[q] > 0){
      for(p in 1:no[q]) {
        if (prior_types[q, p] == 100) {

          lpdf += normal_lpdf(beta[index] | prior[p, 1, q],
                                            prior[p, 2, q]);

        } else if (prior_types[q, p] == 101) {

          lpdf += exp_mod_normal_lpdf(beta[index] | prior[p, 1, q],
                                                    prior[p, 2, q],
                                                    prior[p, 3, q]);

        } else if (prior_types[q, p] == 102) {

          lpdf += skew_normal_lpdf(beta[index] | prior[p, 1, q],
                                                 prior[p, 2, q],
                                                 prior[p, 3, q]);

        } else if (prior_types[q, p] == 103) {

          lpdf += student_t_lpdf(beta[index] | prior[p, 1, q],
                                               prior[p, 2, q],
                                               prior[p, 3, q]);

        } else if (prior_types[q, p] == 104) {

          lpdf += cauchy_lpdf(beta[index] | prior[p, 1, q],
                                            prior[p, 2, q]);

        } else if (prior_types[q, p] == 105) {

          lpdf += double_exponential_lpdf(beta[index] | prior[p, 1, q],
                                                        prior[p, 2, q]);

        } else if (prior_types[q, p] == 106) {

          lpdf += logistic_lpdf(beta[index] | prior[p, 1, q],
                                              prior[p, 2, q]);

        } else if (prior_types[q, p] == 107) {

          lpdf += gumbel_lpdf(beta[index] | prior[p, 1, q],
                                            prior[p, 2, q]);

        }
      }

      index += 1;

    }

  }

  return lpdf;

}

real positive_lpdf(real[] beta , int Q, int[] no, int[,] prior_types, real[,,] prior) {

  int index = 1;
  real lpdf = 0;

  for(q in 1:Q) {
    if(no[q] > 0) {
      for(p in 1:no[q]) {
        if (prior_types[q, p] == 200) {

          lpdf += lognormal_lpdf(beta[index] | prior[p, 1, q],
                                          prior[p, 2, q]);

        } else if (prior_types[q, p] == 201) {

          lpdf += chi_square_lpdf(beta[index] | prior[p, 1, q]);

        } else if (prior_types[q, p] == 202) {

          lpdf += inv_chi_square_lpdf(beta[index] | prior[p, 1, q]);

        } else if (prior_types[q, p] == 203) {

          lpdf += scaled_inv_chi_square_lpdf(beta[index] | prior[p, 1, q],
                                                      prior[p, 2, q]);

        } else if (prior_types[q, p] == 204) {

          lpdf += exponential_lpdf(beta[index] | prior[p, 1, q]);

        } else if (prior_types[q, p] == 205) {

          lpdf += gamma_lpdf(beta[index] | prior[p, 1, q],
                                      prior[p, 2, q]);

        } else if (prior_types[q, p] == 206) {

          lpdf += inv_gamma_lpdf(beta[index] | prior[p, 1, q],
                                          prior[p, 2, q]);

        } else if (prior_types[q, p] == 207) {

          lpdf += weibull_lpdf(beta[index] | prior[p, 1, q],
                                        prior[p, 2, q]);

        } else if (prior_types[q, p] == 208) {

          lpdf += frechet_lpdf(beta[index] | prior[p, 1, q],
                                        prior[p, 2, q]);

        } else if (prior_types[q, p] == 300) {

          lpdf += rayleigh_lpdf(beta[index] | prior[p, 1, q]);

        } else if (prior_types[q, p] == 301) {

          lpdf += wiener_lpdf(beta[index] | prior[p, 1, q],
                                       prior[p, 2, q],
                                       prior[p, 3, q],
                                       prior[p, 4, q]);

        } else if (prior_types[q, p] == 400) {

          lpdf += pareto_lpdf(beta[index] | prior[p, 1, q],
                                       prior[p, 2, q]);

        } else if (prior_types[q, p] == 401) {

          lpdf += pareto_type_2_lpdf(beta[index] | prior[p, 1, q],
                                              prior[p, 2, q],
                                              prior[p, 3, q]);
        }
      }
      index += 1;
    }
  }

  return lpdf;

}

real unit_lpdf(real[] beta , int Q, int[] no, int[,] prior_types, real[,,] prior) {

  int index = 1;
  real lpdf = 0;

  for(q in 1:Q) {
    if(no[q] > 0) {
      for(p in 1:no[q]) {
        if (prior_types[q, p] == 500) {
           lpdf += beta_lcdf(beta[index] | prior[p, 1, q], prior[p, 2, q]);
        }
      }
      index += 1;
    }
  }

  return lpdf;

}
