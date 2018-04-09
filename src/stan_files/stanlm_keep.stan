data {
  int<lower = 0> mp;
  int<lower = 0> n;
  int<lower = 0> link_type;
  int<lower = 0> family;

  int<lower = 0> extra_positive_parameters;
  int<lower = 0> extra_unbounded_parameters;
  int<lower = 0> extra_unit_parameters;

  vector[n] y;

  int<lower = 0> p_unbounded;
  int<lower = 0> p_positive;
  int<lower = 0> p_bounded;

  matrix[n, p_unbounded] x_unbounded;
  matrix[n, p_positive]  x_positive;
  matrix[n, p_bounded]   x_bounded;

  int prior_types_unbounded[p_unbounded];
  int prior_types_positive[p_positive];
  int prior_types_bounded[p_bounded];

  real prior_unbounded[p_unbounded, mp];
  real prior_positive[p_positive, mp];
  real prior_bounded[p_bounded, mp];
}

parameters {
  vector[p_unbounded] beta_unbounded;
  vector<lower = 0>[p_positive] beta_positive;
  vector<lower = 0, upper = 1>[p_bounded] beta_bounded;
  real<lower = 0> sd_;
  real<lower = 0> positive_parameters[extra_positive_parameters];
  real<lower = 0> unbounded_parameters[extra_unbounded_parameters];
  real<lower = 0> unit_parameters[extra_unit_parameters];
}

model {
  vector[n] mean_;

  for(i in 1:n) mean_[i] = 0;

  // Handling of priors.
  if(p_unbounded > 0){
    for(i in 1:p_unbounded) {
      if (prior_types_unbounded[i] == 100) {

        beta_unbounded[i] ~ normal(prior_unbounded[i, 1],
                         prior_unbounded[i, 2]);

      } else if (prior_types_unbounded[i] == 101) {

        beta_unbounded[i] ~ exp_mod_normal(prior_unbounded[i, 1],
                                 prior_unbounded[i, 2],
                                 prior_unbounded[i, 3]);

      } else if (prior_types_unbounded[i] == 102) {

        beta_unbounded[i] ~ skew_normal(prior_unbounded[i, 1],
                              prior_unbounded[i, 2],
                              prior_unbounded[i, 2]);

      } else if (prior_types_unbounded[i] == 103) {

        beta_unbounded[i] ~ student_t(prior_unbounded[i, 1],
                            prior_unbounded[i, 2],
                            prior_unbounded[i, 3]);

      } else if (prior_types_unbounded[i] == 104) {

        beta_unbounded[i] ~ cauchy(prior_unbounded[i, 1],
                         prior_unbounded[i, 2]);

      } else if (prior_types_unbounded[i] == 105) {

        beta_unbounded[i] ~ double_exponential(prior_unbounded[i, 1],
                                     prior_unbounded[i, 2]);

      } else if (prior_types_unbounded[i] == 106) {

        beta_unbounded[i] ~ logistic(prior_unbounded[i, 1],
                           prior_unbounded[i, 2]);

      } else if (prior_types_unbounded[i] == 107) {

        beta_unbounded[i] ~ gumbel(prior_unbounded[i, 1],
                         prior_unbounded[i, 2]);

      }
    }
  }

  if(p_positive > 0) {
    for(i in 1:p_positive) {
      if (prior_types_positive[i] == 200) {

        beta_positive[i] ~ lognormal(prior_positive[i, 1],
                         prior_positive[i, 2]);

      } else if (prior_types_positive[i] == 201) {

        beta_positive[i] ~ chi_square(prior_positive[i, 1]);

      } else if (prior_types_positive[i] == 202) {

        beta_positive[i] ~ inv_chi_square(prior_positive[i, 1]);

      } else if (prior_types_positive[i] == 203) {

        beta_positive[i] ~ scaled_inv_chi_square(prior_positive[i, 1],
                                        prior_positive[i, 2]);

      } else if (prior_types_positive[i] == 204) {

        beta_positive[i] ~ exponential(prior_positive[i, 1]);

      } else if (prior_types_positive[i] == 205) {

        beta_positive[i] ~ gamma(prior_positive[i, 1],
                        prior_positive[i, 2]);

      } else if (prior_types_positive[i] == 206) {

        beta_positive[i] ~ inv_gamma(prior_positive[i, 1],
                            prior_positive[i, 2]);

      } else if (prior_types_positive[i] == 207) {

        beta_positive[i] ~ weibull(prior_positive[i, 1],
                         prior_positive[i, 2]);

      } else if (prior_types_positive[i] == 208) {

        beta_positive[i] ~ frechet(prior_positive[i, 1],
                                   prior_positive[i, 2]);

      } else if (prior_types_positive[i] == 300) {

        beta_positive[i] ~ rayleigh(prior_positive[i, 1]);

      } else if (prior_types_positive[i] == 301) {

        beta_positive[i] ~ wiener(prior_positive[i, 1],
                                  prior_positive[i, 2],
                                  prior_positive[i, 3],
                                  prior_positive[i, 4]);

      } else if (prior_types_positive[i] == 400) {

        beta_positive[i] ~ pareto(prior_positive[i, 1],
                                  prior_positive[i, 2]);

      } else if (prior_types_positive[i] == 401) {

        beta_positive[i] ~ pareto_type_2(prior_positive[i, 1],
                                         prior_positive[i, 2],
                                         prior_positive[i, 3]);
      }
    }
  }

  if(p_bounded > 0) {
    for(i in 1:p_bounded) {
      if (prior_types_bounded[i] == 500) {
         beta_bounded[i] ~ beta(prior_bounded[i, 1],
                                prior_bounded[i, 2]);
      }
    }
  }

  // Handling of the means.
  if(p_bounded > 0)   mean_ = mean_ + x_bounded * beta_bounded;
  if(p_unbounded > 0) mean_ = mean_ + x_unbounded * beta_unbounded;
  if(p_positive > 0)  mean_ = mean_ + x_positive * beta_positive;

  // Handling of links.
  if(link_type == 2) {
    for(i in 1:n) mean_[i] = 1/mean_[i];
  } else if (link_type == 3) {
    for(i in 1:n) mean_[i] = 1/sqrt(mean_[i]);
  } else if (link_type == 4) {
    for(i in 1:n) mean_[i] = exp(mean_[i]);
  } else if (link_type == 8) {
    for(i in 1:n) mean_[i] = mean_[i]^2;
  }

  // The likelihood.
  if(family == 1) {

    // Normal distribution.
    y ~ normal(mean_, sd_);

  } else if (family == 2) {

    // Gumbel distribution.
    real euler_mascheroni = 0.577215664901532;
    real var_ = sd_*sd_;
    real beta = 1/pi()*sqrt(6*var_);
    vector[n] mu = mean_ - beta*euler_mascheroni;
    y ~ gumbel(mu, beta);

  } else if (family == 3) {

    // Skew normal.
    real alpha = unbounded_parameters[1];
    real delta = alpha/sqrt(1 + alpha^2);
    real omega = sd_/sqrt(1 - delta^2*2/pi());
    vector[n] xi = mean_ - omega*(delta*sqrt(2/pi()));

    y ~ skew_normal(xi, omega, alpha);

  }

}
