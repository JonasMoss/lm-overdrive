/**
* Folded normal distribution
*
* @param y Point where the density / cdf is evaluated.
* @param mu Center of the normal distribution.
* @param sigma Standard deviation of the normal.
* @return A real number, the density / cdf at the point.
*/


real fnormal_lpdf(real y, real mu, real sigma) {
  return(log_sum_exp(normal_lpdf(y | mu, sigma),
                     normal_lpdf(y | -mu, sigma)));
}

real fnormal_cdf(real y, real mu, real sigma) {
  return(normal_cdf(y, mu, sigma) + normal_cdf(y, -mu, sigma) - 1);
}

real fnormal_lcdf(real y, real mu, real sigma) {
  return(log(normal_cdf(y, mu, sigma) +
             normal_cdf(y, -mu, sigma) - 1));
}

real fnormal_lccdf(real y, real mu, real sigma) {
  return(log(2 - normal_cdf(y, mu, sigma) - normal_cdf(y, -mu, sigma)));
}

real fnormal_rng(real mu, real sigma) {
  return(fabs(normal_rng(mu, sigma)));
}


/**
* Inverse Gaussian distribution,
*
* @param y Point where the density / cdf is evaluated.
* @param mu Centrality parameter.
* @param lambda Shape parameter.
* @return A real number, the density / cdf at the point.
*/

real inverse_gaussian_lpdf (real y, real mu, real lambda){
  return log((lambda/(2*pi()*(y^3)))^0.5*exp(-lambda*(y - mu)^2/(2*mu^2*y)));
}


/**
* Mixture of a normal and upper truncated normal.
*
* @param y Point where the log density is evaluated.
* @param mu Center of the normal distribution.
* @param upper Upper truncated level.
* @param p Probability of truncation.
* @return A real number, the density of the mixture at y.
*/

real mix_normal_upper_lpdf(real y, real mu, real upper, real p) {

  return log_mix(p, normal_lpdf(y | mu, 1) - normal_lcdf(upper | mu, 1),
                    normal_lpdf(y | mu, 1));
}

/**
* Mixture of a normal and lower truncated normal.
*
* @param y Point where the log density is evaluated.
* @param mu Center of the normal distribution.
* @param lower Lower truncated level.
* @param p Probability of truncation.
* @return A real number, the density of the mixture at y.
*/

real mix_normal_lower_lpdf(real y, real mu, real lower, real p) {

  return log_mix(p, normal_lpdf(y | mu, 1) - normal_lccdf(lower | mu, 1),
                    normal_lpdf(y | mu, 1));
}

/**
* Mixture of a folded normal and lower truncated folded normal.
*
* @param y Point where the log density is evaluated.
* @param mu Center of the normal distribution.
* @param lower Lower truncated level.
* @param p Probability of truncation.
* @return A real number, the density of the mixture at y.
*/

real mix_fnormal_lpdf(real y, real mu, real lower, real p) {

  return log_mix(p, fnormal_lpdf(y | mu, 1) - fnormal_lccdf(lower | mu, 1),
                    fnormal_lpdf(y | mu, 1));
}
