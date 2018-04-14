data {
  int<lower = 0> MAX_PAR; // Maximal number of parameters.
  int<lower = 0> N;       // Number of observations.
  int<lower = 0> P;       // Number of covariates.
  int<lower = 0> Q;       // Number of parameters in the density.
  int<lower = 0> family;  // Integer-coded family.

  real Y[N];                   // Responses.
  real X[N, P];                // Covariate matrix for all parameters.

  // Number of unbounded, positive, unit parameters.
  int<lower = 0> no_unbounded[Q];
  int<lower = 0> no_positive[Q];
  int<lower = 0> no_unit[Q];

  int link_types[Q]; // Array of integer coded link types.

  // Indices telling which covariates belong to which parameter.
  int  positive_indices[Q, P];                          // 0: Not included; 1: included.
  real positive_prior[max(no_positive), MAX_PAR, Q];    // Prior parameters
  int  positive_prior_types[Q, max(no_positive)];       // Integer-coding of prior families.

  int  unbounded_indices[Q, P];
  real unbounded_prior[max(no_unbounded), MAX_PAR, Q];
  int  unbounded_prior_types[Q, max(no_unbounded)];

  int  unit_indices[Q, P];
  real unit_prior[max(no_unit), MAX_PAR, Q];
  int  unit_prior_types[Q, max(no_unit)];

}

parameters {
  real                       beta_unbounded[Q, max(no_unbounded)];
  real<lower = 0>            beta_positive[Q, max(no_positive)];
  real<lower = 0, upper = 1> beta_unit[Q, max(no_unit)];
}

model {
  // Array of transformed parameters.
  int unbounded_index = 1;
  int positive_index = 1;
  int unit_index = 1;
  real params[N, Q];

  for(n in 1:N) {
    for(q in 1:Q) params[n, q] = 0;
  }

  for(q in 1:Q) {
    // Handling of unbounded priors.
    if(no_unbounded[q] > 0){
      for(p in 1:no_unbounded[q]) {
        if (unbounded_prior_types[q, p] == 100) {

          beta_unbounded[q, p] ~ normal(unbounded_prior[p, 1, q],
                                        unbounded_prior[p, 2, q]);

        } else if (unbounded_prior_types[q, p] == 101) {

          beta_unbounded[q, p] ~ exp_mod_normal(unbounded_prior[p, 1, q],
                                               unbounded_prior[p, 2, q],
                                               unbounded_prior[p, 3, q]);

        } else if (unbounded_prior_types[q, p] == 102) {

          beta_unbounded[q, p] ~ skew_normal(unbounded_prior[p, 1, q],
                                            unbounded_prior[p, 2, q],
                                            unbounded_prior[p, 3, q]);

        } else if (unbounded_prior_types[q, p] == 103) {

          beta_unbounded[q, p] ~ student_t(unbounded_prior[p, 1, q],
                                          unbounded_prior[p, 2, q],
                                          unbounded_prior[p, 3, q]);

        } else if (unbounded_prior_types[q, p] == 104) {

          beta_unbounded[q, p] ~ cauchy(unbounded_prior[p, 1, q],
                                       unbounded_prior[p, 2, q]);

        } else if (unbounded_prior_types[q, p] == 105) {

          beta_unbounded[q, p] ~ double_exponential(unbounded_prior[p, 1, q],
                                                   unbounded_prior[p, 2, q]);

        } else if (unbounded_prior_types[q, p] == 106) {

          beta_unbounded[q, p] ~ logistic(unbounded_prior[p, 1, q],
                                         unbounded_prior[p, 2, q]);

        } else if (unbounded_prior_types[q, p] == 107) {

          beta_unbounded[q, p] ~ gumbel(unbounded_prior[p, 1, q],
                                       unbounded_prior[p, 2, q]);

        }
      }
    }

    if(no_positive[q] > 0) {
      for(p in 1:no_positive[q]) {
        if (positive_prior_types[q, p] == 200) {

          beta_positive[q, p] ~ lognormal(positive_prior[p, 1, q],
                                          positive_prior[p, 2, q]);

        } else if (positive_prior_types[q, p] == 201) {

          beta_positive[q, p] ~ chi_square(positive_prior[p, 1, q]);

        } else if (positive_prior_types[q, p] == 202) {

          beta_positive[q, p] ~ inv_chi_square(positive_prior[p, 1, q]);

        } else if (positive_prior_types[q, p] == 203) {

          beta_positive[q, p] ~ scaled_inv_chi_square(positive_prior[p, 1, q],
                                                      positive_prior[p, 2, q]);

        } else if (positive_prior_types[q, p] == 204) {

          beta_positive[q, p] ~ exponential(positive_prior[p, 1, q]);

        } else if (positive_prior_types[q, p] == 205) {

          beta_positive[q, p] ~ gamma(positive_prior[p, 1, q],
                                      positive_prior[p, 2, q]);

        } else if (positive_prior_types[q, p] == 206) {

          beta_positive[q, p] ~ inv_gamma(positive_prior[p, 1, q],
                                          positive_prior[p, 2, q]);

        } else if (positive_prior_types[q, p] == 207) {

          beta_positive[q, p] ~ weibull(positive_prior[p, 1, q],
                                        positive_prior[p, 2, q]);

        } else if (positive_prior_types[q, p] == 208) {

          beta_positive[q, p] ~ frechet(positive_prior[p, 1, q],
                                        positive_prior[p, 2, q]);

        } else if (positive_prior_types[q, p] == 300) {

          beta_positive[q, p] ~ rayleigh(positive_prior[p, 1, q]);

        } else if (positive_prior_types[q, p] == 301) {

          beta_positive[q, p] ~ wiener(positive_prior[p, 1, q],
                                       positive_prior[p, 2, q],
                                       positive_prior[p, 3, q],
                                       positive_prior[p, 4, q]);

        } else if (positive_prior_types[q, p] == 400) {

          beta_positive[q, p] ~ pareto(positive_prior[p, 1, q],
                                       positive_prior[p, 2, q]);

        } else if (positive_prior_types[q, p] == 401) {

          beta_positive[q, p] ~ pareto_type_2(positive_prior[p, 1, q],
                                              positive_prior[p, 2, q],
                                              positive_prior[p, 3, q]);
        }
      }
    }

    if(no_unit[q] > 0) {
      for(p in 1:no_unit[q]) {
        if (positive_prior_types[q, p] == 500) {
           beta_positive[q, p] ~ beta(unit_prior[p, 1, q],
                                      unit_prior[p, 2, q]);
        }
      }
    }

    unbounded_index = 1;
    if(no_unbounded[q] > 0) {
      for(p in 1:P) {
        if(unbounded_indices[q, p] == 1) {
          for(n in 1:N) {
            params[n, q] = params[n, q] + X[n, p] * beta_unbounded[q, unbounded_index];
          }
          unbounded_index += 1;
        }
      }
    }

    positive_index = 1;
    if(no_positive[q] > 0) {
      for(p in 1:P) {
        if(positive_indices[q, p] == 1) {
          for(n in 1:N) {
            params[n, q] = params[n, q] + X[n, p] * beta_positive[q, positive_index];
          }
          positive_index += 1;
        }
      }
    }

    unit_index = 1;
    if(no_unit[q] > 0) {
      for(p in 1:P) {
        if(unit_indices[q, p] == 1) {
          for(n in 1:N) {
            params[n, q] = params[n, q] + X[n, p] * beta_unit[q, unit_index];
          }
          unit_index += 1;
        }
      }
    }

    // Handling of links.
    if(link_types[q] == 2) {
      for(n in 1:N) params[n, q] = 1/params[n, q];
    } else if (link_types[q] == 3) {
      for(n in 1:N) params[n, q] = 1/sqrt(params[n, q]);
    } else if (link_types[q] == 4) {
      for(n in 1:N) params[n, q] = exp(params[n, q]);
    } else if (link_types[q] == 5) {
      for(n in 1:N) params[n, q] = params[n, q]^2;
    }

  }

  // Now for the likelihood:

  if(family == 1) {

    // Normal distribution.
    for(n in 1:N) {
      Y[n] ~ normal(params[n, 1], params[n, 2]);
    }

  } else if (family == 2) {

    for(n in 1:N) {
      // Gumbel distribution.
      real sd_ = params[n, 2];
      real mean_ = params[n, 1];
      real euler_mascheroni = 0.577215664901532;
      real beta = 1/pi()*sqrt(6)*sd_;
      real mu = mean_ - beta*euler_mascheroni;
      Y[n] ~ gumbel(mu, beta);
    }


  } else if (family == 3) {
    for(n in 1:N) {
      // Skew normal.
      real mean_ = params[n, 1];
      real sd_   = params[n, 2];
      real alpha = params[n, 3];

      real delta = alpha/sqrt(1 + alpha^2);
      real omega = sd_/sqrt(1 - delta^2*2/pi());
      real xi = mean_ - omega*(delta*sqrt(2/pi()));

      Y[n] ~ skew_normal(xi, omega, alpha);
    }

  }

}

//
// // Log-priors for coefficients. Runs
//
// void model_lp(int index, vector beta, real[,] prior_parameters) {
//   // Ad all ifs etc here.
//   target += student_t_cdf(beta[i] | prior_parameters[i, 1],
//                                     prior_parameters[i, 3]);
// }
//
//
// if(p_mean_unbounded > 0) {
//   for(index in 1:p_mean_unbounded) {
//     model_lp(index, beta_mean_unbounded, prior_mean_unbounded)
//   }
// }
//
//
// void bounded_lp(
// void unit_lp(
//
//
//
//
//
//   /**
// 24   * Log-prior for baseline hazard parameters
// 25   *
// 26   * @param aux_unscaled Vector (potentially of length 1) of unscaled
// 27   *   auxiliary parameter(s)
// 28   * @param dist Integer specifying the type of prior distribution
// 29   * @param scale Real specifying the scale for the prior distribution
// 30   * @param df Real specifying the df for the prior distribution
// 31   * @return nothing
// 32   */
// 33   void basehaz_lp(vector aux_unscaled, int dist, vector scale, vector df) {
// 34     if (dist > 0) {
// 35       if (dist == 1)
// 36         target += normal_lpdf(aux_unscaled | 0, 1);
// 37       else if (dist == 2)
// 38         target += student_t_lpdf(aux_unscaled | df, 0, 1);
// 39       else
// 40         target += exponential_lpdf(aux_unscaled | 1);
// 41     }
