real likelihood_lpdf (real[] Y, int family, int N, real[ , ] params) {
  real lcdf = 0;

  if(family == 1) {
    // Normal distribution.
    for(n in 1:N) {
      lcdf += normal_lpdf(Y[n] | params[n, 1], params[n, 2]);
    }

  } else if (family == 2) {
    // Gumbel distribution.
    for(n in 1:N) {
      real mean_ = params[n, 1];
      real sd_   = params[n, 2];
      real euler_mascheroni = 0.577215664901532;
      real beta = 1/pi()*sqrt(6)*sd_;
      real mu = mean_ - beta*euler_mascheroni;
      lcdf += gumbel_lpdf(Y[n] | mu, beta);
    }
  } else if (family == 3) {
    // Skew normal.
    for(n in 1:N) {
      real mean_ = params[n, 1];
      real sd_   = params[n, 2];
      real alpha = params[n, 3];

      real delta = alpha/sqrt(1 + alpha^2);
      real omega = sd_/sqrt(1 - delta^2*2/pi());
      real xi = mean_ - omega*(delta*sqrt(2/pi()));

      lcdf += skew_normal_lpdf(Y[n] | xi, omega, alpha);
    }
  } else if (family == 4) {
    // Gamma
    for(n in 1:N) {
      real mean_  = params[n, 1];
      real var_   = params[n, 2]^2;

      real shape  = mean_^2/var_;
      real rate   = mean_/var_;

      lcdf += gamma_lpdf(Y[n] | shape, rate);
    }
  } else if (family == 5) {
    // log-normal
    for(n in 1:N) {
      real mean_ = params[n, 1];
      real var_  = params[n, 2]^2;

      real sigma_sqr = log(var_ + mean_^2) - 2*log(mean_);
      real mu = log(mean_) - sigma_sqr/2;

      lcdf += lognormal_lpdf(Y[n] | mu, sqrt(sigma_sqr));
    }
  } else if(family == 6) {
    // inverse_gaussian
    for(n in 1:N) {
      real mu = params[n, 1];
      real lambda = mu^3/params[n, 2]^2;

      lcdf += inverse_gaussian_lpdf(Y[n] | mu, lambda);
    }
  }

  return lcdf;

}
