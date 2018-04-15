functions {
#include /functions/beta_priors.stan
#include /functions/parameters_array.stan
#include /functions/likelihood.stan
}

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
  real                       beta_unbounded[sum(no_unbounded)];
  real<lower = 0>            beta_positive[sum(no_positive)];
  real<lower = 0, upper = 1> beta_unit[sum(no_unit)];
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

  // Here we handle the prior distributions of the betas.
  beta_unbounded  ~ unbounded(Q, no_unbounded, unbounded_prior_types, unbounded_prior);
  beta_positive   ~ positive(Q, no_positive, positive_prior_types, positive_prior);
  beta_unit       ~ unit(Q, no_unit, unit_prior_types, unit_prior);

  // Now for the likelihood:

  Y ~ likelihood(family, N, params);
}
