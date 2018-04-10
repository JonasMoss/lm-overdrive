data {
  // Basic data.
  int<lower = 0> mp;             // Maximal number of parameters.
  int<lower = 0> n;              // Number of observations.
  vector[n] y;                   // Vector of responses.
  int<lower = 0> family;         // Integer-coded family identifier.

  // Here comes boilerplate code. 'mean', 'sd' and 'probs' use exactly the same
  // code. They have to be declared in their own blocks, hence the boilerplate.

  // Data for the mean vector.
  int<lower = 0> mean_link_type;   // Integer-coded link type.

  int<lower = 0> p_mean_unbounded; // Number of observations with unbounded prior.
  int<lower = 0> p_mean_positive;  // Number of observations with positive prior.
  int<lower = 0> p_mean_unit;      // Number of observations with unit interval prior.

  // Observation matrix corresponding to unbounded priors.
  matrix[n, p_mean_unbounded] x_mean_unbounded;
  matrix[n, p_mean_positive]  x_mean_positive;
  matrix[n, p_mean_unit]      x_mean_unit;

  // Integer-coded prior identifiers for the different classes of priors.
  int prior_types_mean_unbounded[p_mean_unbounded];
  int prior_types_mean_positive[p_mean_positive];
  int prior_types_mean_unit[p_mean_unit];

  // The prior parameters for each prior specified above.
  real prior_mean_unbounded[p_mean_unbounded, mp];
  real prior_mean_positive[p_mean_positive, mp];
  real prior_mean_unit[p_mean_unit, mp];

  // Data for the standard deviation vector. No comments here since everything
  // has the same interpretation as above.
  int<lower = 0> sd_link_type;

  int<lower = 0> p_sd_unbounded;
  int<lower = 0> p_sd_positive;
  int<lower = 0> p_sd_unit;

  matrix[n, p_sd_unbounded] x_sd_unbounded;
  matrix[n, p_sd_positive]  x_sd_positive;
  matrix[n, p_sd_unit]      x_sd_unit;

  int prior_types_sd_unbounded[p_sd_unbounded];
  int prior_types_sd_positive[p_sd_positive];
  int prior_types_sd_unit[p_sd_unit];

  real prior_sd_unbounded[p_sd_unbounded, mp];
  real prior_sd_positive[p_sd_positive, mp];
  real prior_sd_unit[p_sd_unit, mp];

  // Data for the probability vector. No comments here since everything  has the
  // same interpretation as above.
  int<lower = 0> prob_link_type;

  int<lower = 0> p_prob_unbounded;
  int<lower = 0> p_prob_positive;
  int<lower = 0> p_prob_unit;

  matrix[n, p_prob_unbounded] x_prob_unbounded;
  matrix[n, p_prob_positive]  x_prob_positive;
  matrix[n, p_prob_unit]      x_prob_unit;

  int prior_types_prob_unbounded[p_prob_unbounded];
  int prior_types_prob_positive[p_prob_positive];
  int prior_types_prob_unit[p_prob_unit];

  real prior_prob_unbounded[p_prob_unbounded, mp];
  real prior_prob_positive[p_prob_positive, mp];
  real prior_prob_unit[p_prob_unit, mp];

  // Data for extra parameters.

  int<lower = 0> p_extra_unbounded;
  int<lower = 0> p_extra_positive;
  int<lower = 0> p_extra_unit;

  int prior_types_extra_unbounded[p_extra_unbounded];
  int prior_types_extra_positive[p_extra_positive];
  int prior_types_extra_unit[p_extra_unit];

  real prior_extra_unbounded[p_extra_unbounded, mp];
  real prior_extra_positive[p_extra_positive, mp];
  real prior_extra_unit[p_extra_unit, mp];

}

parameters {
  vector[p_mean_unbounded]                   beta_mean_unbounded;
  vector<lower = 0>[p_mean_positive]         beta_mean_positive;
  vector<lower = 0, upper = 1>[p_mean_unit]  beta_mean_unit;

  vector[p_sd_unbounded]                     beta_sd_unbounded;
  vector<lower = 0>[p_sd_positive]           beta_sd_positive;
  vector<lower = 0, upper = 1>[p_sd_unit]    beta_sd_unit;

  vector[p_prob_unbounded]                   beta_prob_unbounded;
  vector<lower = 0>[p_prob_positive]         beta_prob_positive;
  vector<lower = 0, upper = 1>[p_prob_unit]  beta_prob_unit;

  vector[p_extra_unbounded]                  beta_extra_unbounded;
  vector<lower = 0>[p_extra_positive]        beta_extra_positive;
  vector<lower = 0, upper = 1>[p_extra_unit] beta_extra_unit;
}

model {
  //////////////////////////////////////////////////////////////////////////////
  // BOILERPLATE WARNING!
  //
  // Again we have to copy and paste code for mean, sd and probs. I'll show when
  // this boilerplate block stops.
  //////////////////////////////////////////////////////////////////////////////

  vector[n] mean_;
  vector[n] sd_;
  vector[n] prob_;

  //////////
  // MEAN //
  //////////


  for(i in 1:n) mean_[i] = 0;

  // Handling of priors.
  if(p_mean_unbounded > 0){
    for(i in 1:p_mean_unbounded) {
      if (prior_types_mean_unbounded[i] == 100) {

        beta_mean_unbounded[i] ~ normal(prior_mean_unbounded[i, 1],
                                        prior_mean_unbounded[i, 2]);

      } else if (prior_types_mean_unbounded[i] == 101) {

        beta_mean_unbounded[i] ~ exp_mod_normal(prior_mean_unbounded[i, 1],
                                 prior_mean_unbounded[i, 2],
                                 prior_mean_unbounded[i, 3]);

      } else if (prior_types_mean_unbounded[i] == 102) {

        beta_mean_unbounded[i] ~ skew_normal(prior_mean_unbounded[i, 1],
                              prior_mean_unbounded[i, 2],
                              prior_mean_unbounded[i, 2]);

      } else if (prior_types_mean_unbounded[i] == 103) {

        beta_mean_unbounded[i] ~ student_t(prior_mean_unbounded[i, 1],
                                           prior_mean_unbounded[i, 2],
                                           prior_mean_unbounded[i, 3]);

      } else if (prior_types_mean_unbounded[i] == 104) {

        beta_mean_unbounded[i] ~ cauchy(prior_mean_unbounded[i, 1],
                                        prior_mean_unbounded[i, 2]);

      } else if (prior_types_mean_unbounded[i] == 105) {

        beta_mean_unbounded[i] ~ double_exponential(prior_mean_unbounded[i, 1],
                                                    prior_mean_unbounded[i, 2]);

      } else if (prior_types_mean_unbounded[i] == 106) {

        beta_mean_unbounded[i] ~ logistic(prior_mean_unbounded[i, 1],
                                          prior_mean_unbounded[i, 2]);

      } else if (prior_types_mean_unbounded[i] == 107) {

        beta_mean_unbounded[i] ~ gumbel(prior_mean_unbounded[i, 1],
                                        prior_mean_unbounded[i, 2]);

      }
    }
  }

  if(p_mean_positive > 0) {
    for(i in 1:p_mean_positive) {
      if (prior_types_mean_positive[i] == 200) {

        beta_mean_positive[i] ~ lognormal(prior_mean_positive[i, 1],
                                          prior_mean_positive[i, 2]);

      } else if (prior_types_mean_positive[i] == 201) {

        beta_mean_positive[i] ~ chi_square(prior_mean_positive[i, 1]);

      } else if (prior_types_mean_positive[i] == 202) {

        beta_mean_positive[i] ~ inv_chi_square(prior_mean_positive[i, 1]);

      } else if (prior_types_mean_positive[i] == 203) {

        beta_mean_positive[i] ~ scaled_inv_chi_square(prior_mean_positive[i, 1],
                                                      prior_mean_positive[i, 2]);

      } else if (prior_types_mean_positive[i] == 204) {

        beta_mean_positive[i] ~ exponential(prior_mean_positive[i, 1]);

      } else if (prior_types_mean_positive[i] == 205) {

        beta_mean_positive[i] ~ gamma(prior_mean_positive[i, 1],
                                      prior_mean_positive[i, 2]);

      } else if (prior_types_mean_positive[i] == 206) {

        beta_mean_positive[i] ~ inv_gamma(prior_mean_positive[i, 1],
                                          prior_mean_positive[i, 2]);

      } else if (prior_types_mean_positive[i] == 207) {

        beta_mean_positive[i] ~ weibull(prior_mean_positive[i, 1],
                                        prior_mean_positive[i, 2]);

      } else if (prior_types_mean_positive[i] == 208) {

        beta_mean_positive[i] ~ frechet(prior_mean_positive[i, 1],
                                        prior_mean_positive[i, 2]);

      } else if (prior_types_mean_positive[i] == 300) {

        beta_mean_positive[i] ~ rayleigh(prior_mean_positive[i, 1]);

      } else if (prior_types_mean_positive[i] == 301) {

        beta_mean_positive[i] ~ wiener(prior_mean_positive[i, 1],
                                       prior_mean_positive[i, 2],
                                       prior_mean_positive[i, 3],
                                       prior_mean_positive[i, 4]);

      } else if (prior_types_mean_positive[i] == 400) {

        beta_mean_positive[i] ~ pareto(prior_mean_positive[i, 1],
                                       prior_mean_positive[i, 2]);

      } else if (prior_types_mean_positive[i] == 401) {

        beta_mean_positive[i] ~ pareto_type_2(prior_mean_positive[i, 1],
                                              prior_mean_positive[i, 2],
                                              prior_mean_positive[i, 3]);
      }
    }
  }

  if(p_mean_unit > 0) {
    for(i in 1:p_mean_unit) {
      if (prior_types_mean_unit[i] == 500) {
         beta_mean_unit[i] ~ beta(prior_mean_unit[i, 1],
                                     prior_mean_unit[i, 2]);
      }
    }
  }

  // Handling of the means.
  if(p_mean_unit > 0)   mean_ = mean_ + x_mean_unit * beta_mean_unit;
  if(p_mean_unbounded > 0) mean_ = mean_ + x_mean_unbounded * beta_mean_unbounded;
  if(p_mean_positive > 0)  mean_ = mean_ + x_mean_positive * beta_mean_positive;

  // Handling of links.
  if(mean_link_type == 2) {
    for(i in 1:n) mean_[i] = 1/mean_[i];
  } else if (mean_link_type == 3) {
    for(i in 1:n) mean_[i] = 1/sqrt(mean_[i]);
  } else if (mean_link_type == 4) {
    for(i in 1:n) mean_[i] = exp(mean_[i]);
  } else if (mean_link_type == 5) {
    for(i in 1:n) mean_[i] = mean_[i]^2;
  }

  //////////
  // SD   //
  //////////

  for(i in 1:n) sd_[i] = 0;

  // Handling of priors.
  if(p_sd_unbounded > 0){
    for(i in 1:p_sd_unbounded) {
      if (prior_types_sd_unbounded[i] == 100) {

        beta_sd_unbounded[i] ~ normal(prior_sd_unbounded[i, 1],
                                        prior_sd_unbounded[i, 2]);

      } else if (prior_types_sd_unbounded[i] == 101) {

        beta_sd_unbounded[i] ~ exp_mod_normal(prior_sd_unbounded[i, 1],
                                 prior_sd_unbounded[i, 2],
                                 prior_sd_unbounded[i, 3]);

      } else if (prior_types_sd_unbounded[i] == 102) {

        beta_sd_unbounded[i] ~ skew_normal(prior_sd_unbounded[i, 1],
                              prior_sd_unbounded[i, 2],
                              prior_sd_unbounded[i, 2]);

      } else if (prior_types_sd_unbounded[i] == 103) {

        beta_sd_unbounded[i] ~ student_t(prior_sd_unbounded[i, 1],
                                           prior_sd_unbounded[i, 2],
                                           prior_sd_unbounded[i, 3]);

      } else if (prior_types_sd_unbounded[i] == 104) {

        beta_sd_unbounded[i] ~ cauchy(prior_sd_unbounded[i, 1],
                                        prior_sd_unbounded[i, 2]);

      } else if (prior_types_sd_unbounded[i] == 105) {

        beta_sd_unbounded[i] ~ double_exponential(prior_sd_unbounded[i, 1],
                                                    prior_sd_unbounded[i, 2]);

      } else if (prior_types_sd_unbounded[i] == 106) {

        beta_sd_unbounded[i] ~ logistic(prior_sd_unbounded[i, 1],
                                          prior_sd_unbounded[i, 2]);

      } else if (prior_types_sd_unbounded[i] == 107) {

        beta_sd_unbounded[i] ~ gumbel(prior_sd_unbounded[i, 1],
                                        prior_sd_unbounded[i, 2]);

      }
    }
  }

  if(p_sd_positive > 0) {
    for(i in 1:p_sd_positive) {
      if (prior_types_sd_positive[i] == 200) {

        beta_sd_positive[i] ~ lognormal(prior_sd_positive[i, 1],
                                          prior_sd_positive[i, 2]);

      } else if (prior_types_sd_positive[i] == 201) {

        beta_sd_positive[i] ~ chi_square(prior_sd_positive[i, 1]);

      } else if (prior_types_sd_positive[i] == 202) {

        beta_sd_positive[i] ~ inv_chi_square(prior_sd_positive[i, 1]);

      } else if (prior_types_sd_positive[i] == 203) {

        beta_sd_positive[i] ~ scaled_inv_chi_square(prior_sd_positive[i, 1],
                                                      prior_sd_positive[i, 2]);

      } else if (prior_types_sd_positive[i] == 204) {

        beta_sd_positive[i] ~ exponential(prior_sd_positive[i, 1]);

      } else if (prior_types_sd_positive[i] == 205) {

        beta_sd_positive[i] ~ gamma(prior_sd_positive[i, 1],
                                      prior_sd_positive[i, 2]);

      } else if (prior_types_sd_positive[i] == 206) {

        beta_sd_positive[i] ~ inv_gamma(prior_sd_positive[i, 1],
                                          prior_sd_positive[i, 2]);

      } else if (prior_types_sd_positive[i] == 207) {

        beta_sd_positive[i] ~ weibull(prior_sd_positive[i, 1],
                                        prior_sd_positive[i, 2]);

      } else if (prior_types_sd_positive[i] == 208) {

        beta_sd_positive[i] ~ frechet(prior_sd_positive[i, 1],
                                        prior_sd_positive[i, 2]);

      } else if (prior_types_sd_positive[i] == 300) {

        beta_sd_positive[i] ~ rayleigh(prior_sd_positive[i, 1]);

      } else if (prior_types_sd_positive[i] == 301) {

        beta_sd_positive[i] ~ wiener(prior_sd_positive[i, 1],
                                       prior_sd_positive[i, 2],
                                       prior_sd_positive[i, 3],
                                       prior_sd_positive[i, 4]);

      } else if (prior_types_sd_positive[i] == 400) {

        beta_sd_positive[i] ~ pareto(prior_sd_positive[i, 1],
                                       prior_sd_positive[i, 2]);

      } else if (prior_types_sd_positive[i] == 401) {

        beta_sd_positive[i] ~ pareto_type_2(prior_sd_positive[i, 1],
                                              prior_sd_positive[i, 2],
                                              prior_sd_positive[i, 3]);
      }
    }
  }

  if(p_sd_unit > 0) {
    for(i in 1:p_sd_unit) {
      if (prior_types_sd_unit[i] == 500) {
         beta_sd_unit[i] ~ beta(prior_sd_unit[i, 1],
                                     prior_sd_unit[i, 2]);
      }
    }
  }

  // Handling of the means.
  if(p_sd_unit > 0)   sd_ = sd_ + x_sd_unit * beta_sd_unit;
  if(p_sd_unbounded > 0) sd_ = sd_ + x_sd_unbounded * beta_sd_unbounded;
  if(p_sd_positive > 0)  sd_ = sd_ + x_sd_positive * beta_sd_positive;

  // Handling of links.
  if(sd_link_type == 2) {
    for(i in 1:n) sd_[i] = 1/sd_[i];
  } else if (sd_link_type == 3) {
    for(i in 1:n) sd_[i] = 1/sqrt(sd_[i]);
  } else if (sd_link_type == 4) {
    for(i in 1:n) sd_[i] = exp(sd_[i]);
  } else if (sd_link_type == 5) {
    for(i in 1:n) sd_[i] = sd_[i]^2;
  }


  ///////////
  // PROBS //
  ///////////

  for(i in 1:n) prob_[i] = 0;

  // Handling of priors.
  if(p_prob_unbounded > 0){
    for(i in 1:p_prob_unbounded) {
      if (prior_types_prob_unbounded[i] == 100) {

        beta_prob_unbounded[i] ~ normal(prior_prob_unbounded[i, 1],
                                        prior_prob_unbounded[i, 2]);

      } else if (prior_types_prob_unbounded[i] == 101) {

        beta_prob_unbounded[i] ~ exp_mod_normal(prior_prob_unbounded[i, 1],
                                 prior_prob_unbounded[i, 2],
                                 prior_prob_unbounded[i, 3]);

      } else if (prior_types_prob_unbounded[i] == 102) {

        beta_prob_unbounded[i] ~ skew_normal(prior_prob_unbounded[i, 1],
                              prior_prob_unbounded[i, 2],
                              prior_prob_unbounded[i, 2]);

      } else if (prior_types_prob_unbounded[i] == 103) {

        beta_prob_unbounded[i] ~ student_t(prior_prob_unbounded[i, 1],
                                           prior_prob_unbounded[i, 2],
                                           prior_prob_unbounded[i, 3]);

      } else if (prior_types_prob_unbounded[i] == 104) {

        beta_prob_unbounded[i] ~ cauchy(prior_prob_unbounded[i, 1],
                                        prior_prob_unbounded[i, 2]);

      } else if (prior_types_prob_unbounded[i] == 105) {

        beta_prob_unbounded[i] ~ double_exponential(prior_prob_unbounded[i, 1],
                                                    prior_prob_unbounded[i, 2]);

      } else if (prior_types_prob_unbounded[i] == 106) {

        beta_prob_unbounded[i] ~ logistic(prior_prob_unbounded[i, 1],
                                          prior_prob_unbounded[i, 2]);

      } else if (prior_types_prob_unbounded[i] == 107) {

        beta_prob_unbounded[i] ~ gumbel(prior_prob_unbounded[i, 1],
                                        prior_prob_unbounded[i, 2]);

      }
    }
  }

  if(p_prob_positive > 0) {
    for(i in 1:p_prob_positive) {
      if (prior_types_prob_positive[i] == 200) {

        beta_prob_positive[i] ~ lognormal(prior_prob_positive[i, 1],
                                          prior_prob_positive[i, 2]);

      } else if (prior_types_prob_positive[i] == 201) {

        beta_prob_positive[i] ~ chi_square(prior_prob_positive[i, 1]);

      } else if (prior_types_prob_positive[i] == 202) {

        beta_prob_positive[i] ~ inv_chi_square(prior_prob_positive[i, 1]);

      } else if (prior_types_prob_positive[i] == 203) {

        beta_prob_positive[i] ~ scaled_inv_chi_square(prior_prob_positive[i, 1],
                                                      prior_prob_positive[i, 2]);

      } else if (prior_types_prob_positive[i] == 204) {

        beta_prob_positive[i] ~ exponential(prior_prob_positive[i, 1]);

      } else if (prior_types_prob_positive[i] == 205) {

        beta_prob_positive[i] ~ gamma(prior_prob_positive[i, 1],
                                      prior_prob_positive[i, 2]);

      } else if (prior_types_prob_positive[i] == 206) {

        beta_prob_positive[i] ~ inv_gamma(prior_prob_positive[i, 1],
                                          prior_prob_positive[i, 2]);

      } else if (prior_types_prob_positive[i] == 207) {

        beta_prob_positive[i] ~ weibull(prior_prob_positive[i, 1],
                                        prior_prob_positive[i, 2]);

      } else if (prior_types_prob_positive[i] == 208) {

        beta_prob_positive[i] ~ frechet(prior_prob_positive[i, 1],
                                        prior_prob_positive[i, 2]);

      } else if (prior_types_prob_positive[i] == 300) {

        beta_prob_positive[i] ~ rayleigh(prior_prob_positive[i, 1]);

      } else if (prior_types_prob_positive[i] == 301) {

        beta_prob_positive[i] ~ wiener(prior_prob_positive[i, 1],
                                       prior_prob_positive[i, 2],
                                       prior_prob_positive[i, 3],
                                       prior_prob_positive[i, 4]);

      } else if (prior_types_prob_positive[i] == 400) {

        beta_prob_positive[i] ~ pareto(prior_prob_positive[i, 1],
                                       prior_prob_positive[i, 2]);

      } else if (prior_types_prob_positive[i] == 401) {

        beta_prob_positive[i] ~ pareto_type_2(prior_prob_positive[i, 1],
                                              prior_prob_positive[i, 2],
                                              prior_prob_positive[i, 3]);
      }
    }
  }

  if(p_prob_unit > 0) {
    for(i in 1:p_prob_unit) {
      if (prior_types_prob_unit[i] == 500) {
         beta_prob_unit[i] ~ beta(prior_prob_unit[i, 1],
                                     prior_prob_unit[i, 2]);
      }
    }
  }

  // Handling of the means.
  if(p_prob_unit > 0)   prob_ = prob_ + x_prob_unit * beta_prob_unit;
  if(p_prob_unbounded > 0) prob_ = prob_ + x_prob_unbounded * beta_prob_unbounded;
  if(p_prob_positive > 0)  prob_ = prob_ + x_prob_positive * beta_prob_positive;

  // Handling of links.
  if(prob_link_type == 2) {
    for(i in 1:n) prob_[i] = 1/prob_[i];
  } else if (prob_link_type == 3) {
    for(i in 1:n) prob_[i] = 1/sqrt(prob_[i]);
  } else if (prob_link_type == 4) {
    for(i in 1:n) prob_[i] = exp(prob_[i]);
  } else if (prob_link_type == 5) {
    for(i in 1:n) prob_[i] = prob_[i]^2;
  }


  //////////////////////
  // Extra parameters //
  //////////////////////

  // Handling of priors.
  if(p_extra_unbounded > 0){
    for(i in 1:p_extra_unbounded) {
      if (prior_types_extra_unbounded[i] == 100) {

        beta_extra_unbounded[i] ~ normal(prior_extra_unbounded[i, 1],
                                        prior_extra_unbounded[i, 2]);

      } else if (prior_types_extra_unbounded[i] == 101) {

        beta_extra_unbounded[i] ~ exp_mod_normal(prior_extra_unbounded[i, 1],
                                 prior_extra_unbounded[i, 2],
                                 prior_extra_unbounded[i, 3]);

      } else if (prior_types_extra_unbounded[i] == 102) {

        beta_extra_unbounded[i] ~ skew_normal(prior_extra_unbounded[i, 1],
                              prior_extra_unbounded[i, 2],
                              prior_extra_unbounded[i, 2]);

      } else if (prior_types_extra_unbounded[i] == 103) {

        beta_extra_unbounded[i] ~ student_t(prior_extra_unbounded[i, 1],
                                           prior_extra_unbounded[i, 2],
                                           prior_extra_unbounded[i, 3]);

      } else if (prior_types_extra_unbounded[i] == 104) {

        beta_extra_unbounded[i] ~ cauchy(prior_extra_unbounded[i, 1],
                                        prior_extra_unbounded[i, 2]);

      } else if (prior_types_extra_unbounded[i] == 105) {

        beta_extra_unbounded[i] ~ double_exponential(prior_extra_unbounded[i, 1],
                                                    prior_extra_unbounded[i, 2]);

      } else if (prior_types_extra_unbounded[i] == 106) {

        beta_extra_unbounded[i] ~ logistic(prior_extra_unbounded[i, 1],
                                          prior_extra_unbounded[i, 2]);

      } else if (prior_types_extra_unbounded[i] == 107) {

        beta_extra_unbounded[i] ~ gumbel(prior_extra_unbounded[i, 1],
                                        prior_extra_unbounded[i, 2]);

      }
    }
  }

  if(p_extra_positive > 0) {
    for(i in 1:p_extra_positive) {
      if (prior_types_extra_positive[i] == 200) {

        beta_extra_positive[i] ~ lognormal(prior_extra_positive[i, 1],
                                          prior_extra_positive[i, 2]);

      } else if (prior_types_extra_positive[i] == 201) {

        beta_extra_positive[i] ~ chi_square(prior_extra_positive[i, 1]);

      } else if (prior_types_extra_positive[i] == 202) {

        beta_extra_positive[i] ~ inv_chi_square(prior_extra_positive[i, 1]);

      } else if (prior_types_extra_positive[i] == 203) {

        beta_extra_positive[i] ~ scaled_inv_chi_square(prior_extra_positive[i, 1],
                                                      prior_extra_positive[i, 2]);

      } else if (prior_types_extra_positive[i] == 204) {

        beta_extra_positive[i] ~ exponential(prior_extra_positive[i, 1]);

      } else if (prior_types_extra_positive[i] == 205) {

        beta_extra_positive[i] ~ gamma(prior_extra_positive[i, 1],
                                      prior_extra_positive[i, 2]);

      } else if (prior_types_extra_positive[i] == 206) {

        beta_extra_positive[i] ~ inv_gamma(prior_extra_positive[i, 1],
                                          prior_extra_positive[i, 2]);

      } else if (prior_types_extra_positive[i] == 207) {

        beta_extra_positive[i] ~ weibull(prior_extra_positive[i, 1],
                                        prior_extra_positive[i, 2]);

      } else if (prior_types_extra_positive[i] == 208) {

        beta_extra_positive[i] ~ frechet(prior_extra_positive[i, 1],
                                        prior_extra_positive[i, 2]);

      } else if (prior_types_extra_positive[i] == 300) {

        beta_extra_positive[i] ~ rayleigh(prior_extra_positive[i, 1]);

      } else if (prior_types_extra_positive[i] == 301) {

        beta_extra_positive[i] ~ wiener(prior_extra_positive[i, 1],
                                       prior_extra_positive[i, 2],
                                       prior_extra_positive[i, 3],
                                       prior_extra_positive[i, 4]);

      } else if (prior_types_extra_positive[i] == 400) {

        beta_extra_positive[i] ~ pareto(prior_extra_positive[i, 1],
                                       prior_extra_positive[i, 2]);

      } else if (prior_types_extra_positive[i] == 401) {

        beta_extra_positive[i] ~ pareto_type_2(prior_extra_positive[i, 1],
                                              prior_extra_positive[i, 2],
                                              prior_extra_positive[i, 3]);
      }
    }
  }

  if(p_extra_unit > 0) {
    for(i in 1:p_extra_unit) {
      if (prior_types_extra_unit[i] == 500) {
         beta_extra_unit[i] ~ beta(prior_extra_unit[i, 1],
                                   prior_extra_unit[i, 2]);
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // BOILERPLATE END
  //////////////////////////////////////////////////////////////////////////////

  // The likelihood.
  if(family == 1) {

    // Normal distribution.
    y ~ normal(mean_, sd_);

  } else if (family == 2) {

    // Gumbel distribution.
    real euler_mascheroni = 0.577215664901532;
    vector[n] beta = 1/pi()*sqrt(6)*sd_;
    vector[n] mu = mean_ - beta*euler_mascheroni;
    y ~ gumbel(mu, beta);

  } else if (family == 3) {

    // Skew normal.
    real alpha = beta_extra_unbounded[1];
    real delta = alpha/sqrt(1 + alpha^2);
    vector[n] omega = sd_/sqrt(1 - delta^2*2/pi());
    vector[n] xi = mean_ - omega*(delta*sqrt(2/pi()));

    y ~ skew_normal(xi, omega, alpha);

  }

}
