functions {
#include /chunks/densities.stan
#include /chunks/beta_priors.stan
#include /chunks/parameters_array.stan
#include /chunks/likelihood.stan
#include /chunks/distributions.stan

}

data {
  int<lower = 0> MAX_PAR;     // Maximal number of parameters.
  int<lower = 0> N_unbounded; // Number of observations.
  int<lower = 0> N_positive;  // Number of observations.
  int<lower = 0> N_unit;      // Number of observations.
  int<lower = 0> N;           // Number of observations.
  int<lower = 0> P;           // Number of covariates.
  int<lower = 0> Q;           // Number of parameters in the density.

  int<lower = 0> family;      // Integer-coded family.
  int<lower = 0> family_type; // Domain of the family. 0: Unbounded.
  int link_types[Q];          // Array of integer coded link types.

  real M[N];               // Sample sizes / degrees of freedom.
  real lower_bounds[N];    // Vector of lower cut-offs.
  real upper_bounds[N];    // Vector of upper cut-offs.
  real Z[N];               // Responses.
  real X[N, P];            // Covariate matrix for all parameters.

  // Index vector. If dist_indices[n] = j, the nth observation belongs to the
  // jth distribution.

  int dist_indices[N];

  // The followong inputs concern the beta parameters. The large number of input
  // data is here because STAN requires knowledge about the whether a parameter
  // is unbounded, positive or on the unit interval in order to work well.

  // Number of unbounded, positive, unit parameters.
  int<lower = 0> no_unbounded[Q];
  int<lower = 0> no_positive[Q];
  int<lower = 0> no_unit[Q];

  // 0: Not included; 1: included.
  int unbounded_indices[Q, P];
  int positive_indices[Q, P];
  int unit_indices[Q, P];

  // Prior parameters
  real unbounded_prior[max(no_unbounded), MAX_PAR, Q];
  real positive_prior[max(no_positive), MAX_PAR, Q];
  real unit_prior[max(no_unit), MAX_PAR, Q];

  // Integer-coding of prior families.
  int positive_prior_types[Q, max(no_positive)];
  int unbounded_prior_types[Q, max(no_unbounded)];
  int unit_prior_types[Q, max(no_unit)];

}

transformed data {
  real SQRT_M[N] = sqrt(M);
}

parameters {
  // The betas are split into three vectors.
  real                       beta_unbounded[sum(no_unbounded)];
  real<lower = 0>            beta_positive[sum(no_positive)];
  real<lower = 0, upper = 1> beta_unit[sum(no_unit)];

  // The unobserved thetas. They are split in three due to numerics.
  real                       thetas_unbounded[N_unbounded];
  real<lower = 0>            thetas_positive[N_positive];
  real<lower = 0, upper = 1> thetas_unit[N_unit];
}

model {
  // Array of transformed parameters. Here the 'params' array is constructed
  // and the link transformations are carried out. The params array is used
  // in the likelihood. 'params' should be thought of as g^(-1)('beta %*% X')
  // for all the different links and betas, and associated Xs.

  real params[N, Q] = get_parameters(N, Q, P,
                        no_unbounded, no_positive, no_unit,
                        beta_unbounded, beta_positive, beta_unit,
                        unbounded_indices, positive_indices, unit_indices,
                        X, link_types);

  // This isn't used if p is never accessed.
  real p[N] = params[ , Q];

  // Here we handle the prior distributions of the betas.
  beta_unbounded  ~ unbounded(Q, no_unbounded, unbounded_prior_types, unbounded_prior);
  beta_positive   ~ positive(Q, no_positive, positive_prior_types, positive_prior);
  beta_unit       ~ unit(Q, no_unit, unit_prior_types, unit_prior);

  // We handle the thetas.
  if(family_type == 0) {
    thetas_unbounded ~ likelihood(family, N, params);
  } else if(family_type == 1) {
    thetas_positive ~ likelihood(family, N, params);
  } else if(family_type == 2) {
    thetas_unit ~ likelihood(family, N, params);
  }

  // Finally the zs are modelled. We split this into three to handle the sign
  // constraints.

  if(family_type == 0) {
    Z ~ distributions(N, thetas_unbounded, SQRT_M, lower_bounds, upper_bounds, dist_indices, p);
  } else if(family_type == 1) {
    Z ~ distributions(N, thetas_positive, SQRT_M, lower_bounds, upper_bounds, dist_indices, p);
  } else if(family_type == 2) {
    Z ~ distributions(N, thetas_unit, SQRT_M, lower_bounds, upper_bounds, dist_indices, p);
  }

}
