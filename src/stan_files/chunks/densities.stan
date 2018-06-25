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

real fnormal_ccdf(real y, real mu, real sigma) {
  return(2 - normal_cdf(y, mu, sigma) - normal_cdf(y, -mu, sigma));
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
* Truncated (folded) normal distribution
*
* Log-densities for lower and upper truncated (folded) normals.
*
* @param y Point where the density / cdf is evaluated.
* @param mu Center of the normal distribution.
* @param sigma Standard deviation of the normal.
* @param lower Truncation point
* @return A real number, the density / cdf at the point.
*/

real lower_fnormal_lpdf(real y, real mu, real sigma, real lower) {
  return(fnormal_lpdf(y | mu, 1) - fnormal_lccdf(lower | mu, 1));
}

real upper_fnormal_lpdf(real y, real mu, real sigma, real upper) {
  return(fnormal_lpdf(y | mu, 1) - fnormal_lcdf(upper | mu, 1));
}

real inner_fnormal_lpdf(real y, real mu, real sigma, real lower, real upper) {
  return(fnormal_lpdf(y | mu, 1) -
         log(fnormal_cdf(upper, mu, sigma) -
             fnormal_cdf(lower, mu, sigma)));
}

real double_fnormal_lpdf(real y, real mu, real sigma, real lower, real upper) {
  return(fnormal_lpdf(y | mu, 1) -
         log(fnormal_cdf(upper, mu, sigma) + fnormal_ccdf(lower, mu, sigma)));
}

real lower_normal_lpdf(real y, real mu, real sigma, real lower) {
  return(normal_lpdf(y | mu, 1) - normal_lccdf(lower | mu, 1));
}

real upper_normal_lpdf(real y, real mu, real sigma, real upper) {
  return(normal_lpdf(y | mu, 1) - normal_lcdf(upper | mu, 1));
}

real inner_normal_lpdf(real y, real mu, real sigma, real lower, real upper) {
  return(normal_lpdf(y | mu, 1) -
         log(normal_cdf(upper, mu, sigma) -
             normal_cdf(lower, mu, sigma)));
}

real double_normal_lpdf(real y, real mu, real sigma, real lower, real upper) {
  return(normal_lpdf(y | mu, 1) -
         log(normal_cdf(upper, mu, sigma) + normal_cdf(-lower, -mu, sigma)));
}

/**
* Mixture of a normal and upper truncated (folded) normal
*
* Log-densities for truncated (folded) normals.
*
* @param y Point where the log density is evaluated.
* @param mu Center of the normal distribution.
* @param upper Upper truncated level.
* @param p Probability of truncation.
* @return A real number, the density of the mixture at y.
*/

real mix_fnormal_lower_lpdf(real y, real mu, real lower, real p) {
  return log_mix(p, lower_fnormal_lpdf(y | mu, 1, lower),
                    fnormal_lpdf(y | mu, 1));
}

real mix_fnormal_upper_lpdf(real y, real mu, real upper, real p) {
  return log_mix(p, upper_fnormal_lpdf(y | mu, 1, upper),
                    fnormal_lpdf(y | mu, 1));
}

real mix_fnormal_double_lpdf(real y, real mu, real lower, real upper, real p) {
  return log_mix(p, double_normal_lpdf(y | mu, 1, lower, upper),
                    fnormal_lpdf(y | mu, 1));
}

real mix_fnormal_inner_lpdf(real y, real mu, real lower, real upper, real p) {
  return log_mix(p, inner_fnormal_lpdf(y | mu, 1, lower, upper),
                    fnormal_lpdf(y | mu, 1));
}

real mix_normal_lower_lpdf(real y, real mu, real lower, real p) {

  return log_mix(p, lower_fnormal_lpdf(y | mu, 1, lower),
                    normal_lpdf(y | mu, 1));
}

real mix_normal_upper_lpdf(real y, real mu, real upper, real p) {
  return log_mix(p, upper_normal_lpdf(y | mu, 1, upper),
                    normal_lpdf(y | mu, 1));
}

real mix_normal_double_lpdf(real y, real mu, real lower, real upper, real p) {
  return log_mix(p, double_normal_lpdf(y | mu, 1, lower, upper),
                    normal_lpdf(y | mu, 1));
}

real mix_normal_inner_lpdf(real y, real mu, real lower, real upper, real p) {
  return log_mix(p, inner_normal_lpdf(y | mu, 1, lower, upper),
                    normal_lpdf(y | mu, 1));
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
  return (0.5*(log(lambda) - log(2) - log(pi()) - 3*log(y)) -
         lambda*(y - mu)^2/(2*mu^2*y));
}
