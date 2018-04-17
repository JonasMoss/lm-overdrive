## =============================================================================
## Simulation test 1: Normal with (2 + 1 + 1) parameters.
## Effect size distribution: Normal
## =============================================================================

set.seed(1337)
N = 100
M = sample(N, 20:80, replace = TRUE)
x1 = rexp(N, 5)
p = rbinom(N, 1, 0.7)
mean_ = 0.1 + 0.1*x1
sd_   = 0.1
thetas = rnorm(N, mean = mean_, sd = sd_)
lower = rep(1.96, N)
upper = rep(0, N)
z = rnorm(N, sqrt(M)*thetas, 1)*(1 - p) +
  truncnorm::rtruncnorm(N, mean = sqrt(M)*thetas, sd = 1, a = lower)*p

dist_indices = rep(3, N)
dist_indices[z < 1.96] = 7


data = data.frame(z = z, x1 = x1, lower = lower, upper = upper,
                  dist_indices = dist_indices, M = M)

formula = z ~ normal(mean ~ 1 + x1, sd ~ 1, p ~ 1)
priors = list(mean = list((Intercept) ~ gamma(1, 1),
                          x1 ~ gamma(1, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p    = list((Intercept) ~ beta(1, 1)))


straussR(formula = formula, priors = priors, data = data, chains = 1,
        control = list(adapt_delta = 0.99)) -> mods

library("magrittr")
rstan::extract(mods)$beta_positive[ , 1] %>% hist


## =============================================================================
## Simulation test 2: Folded normal with (1 + 1 + 1) parameters.
## Effect size distribution: Gamma
## =============================================================================

set.seed(1337)
N = 100
M = sample(N, 20:80, replace = TRUE)
x1 = rexp(N, 5)
p = rbinom(N, 1, 0.7)
mean_ = 0.1 + 0.2*x1
sd_   = 0.1
thetas = rgamma(N, mean_^2/sd_^2, mean_/sd_^2)
lower = rep(1.96, N)
upper = rep(0, N)
z = abs(rnorm(N, sqrt(M)*thetas, 1)*(1 - p) +
          truncnorm::rtruncnorm(N, mean = sqrt(M)*thetas, sd = 1, a = lower)*p)

dist_indices = rep(1, N)
dist_indices[z < 1.96] = 5

data = data.frame(z = z, x1 = x1, lower = lower, upper = upper,
                  dist_indices = dist_indices, M = M)

formula = z ~ gamma(mean ~ 1 + x1, sd ~ 1, p ~ 1)
priors = list(mean = list((Intercept) ~ gamma(1, 1),
                          x1 ~ gamma(1, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p    = list((Intercept) ~ beta(1, 1)))


strauss(formula = formula, priors = priors, data = data, chains = 1,
        control = list(adapt_delta = 0.99)) -> mods

library("magrittr")
rstan::extract(mods)$beta_positive[ , 1] %>% hist
plot(thetas, colMeans(rstan::extract(mods)$thetas_positive))


## =============================================================================
## Simulation test 3: Folded normal with (1 + 1 + 1) parameters.
## Effect size distribution: Inverse-Gaussian
## =============================================================================

set.seed(1337)
N = 1000
M = sample(N, 20:80, replace = TRUE)
x1 = rexp(N, 5)
p = rbinom(N, 1, 0.7)
mean_ = 0.1
sd_   = 0.1
thetas = rgamma(N, mean_^2/sd_^2, mean_/sd_^2)
lower = rep(1.96, N)
upper = rep(0, N)
z = abs(rnorm(N, sqrt(M)*thetas, 1)*(1 - p) +
          truncnorm::rtruncnorm(N, mean = sqrt(M)*thetas, sd = 1, a = lower)*p)

dist_indices = rep(1, N)
dist_indices[z < 1.96] = 5

data = data.frame(z = z, x1 = x1, lower = lower, upper = upper,
                  dist_indices = dist_indices, M = M)

formula = z ~ inverse_gaussian(mean ~ 1, sd ~ 1, p ~ 1)
priors = list(mean = list((Intercept) ~ gamma(1, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p    = list((Intercept) ~ beta(1, 1)))


strauss(formula = formula, priors = priors, data = data, chains = 1,
        control = list(adapt_delta = 0.99)) -> mods

library("magrittr")
rstan::extract(mods)$beta_positive[ , 1] %>% hist


## =============================================================================
## Simulation test 4: Normal with (2 + 2 + 1) parameters.
## Effect size distribution: Normal
## =============================================================================

set.seed(1337)
N = 100
M = sample(N, 20:80, replace = TRUE)
x1 = rexp(N, 5)
p = rbinom(N, 1, 0.7)
mean_ = 0.1 + 0.1*x1
sd_   = 0.1 + x1
thetas = rnorm(N, mean = mean_, sd = sd_)
lower = rep(1.96, N)
upper = rep(0, N)
z = rnorm(N, sqrt(M)*thetas, 1)*(1 - p) +
  truncnorm::rtruncnorm(N, mean = sqrt(M)*thetas, sd = 1, a = lower)*p

dist_indices = rep(3, N)
dist_indices[z < 1.96] = 7


data = data.frame(z = z, x1 = x1, lower = lower, upper = upper,
                  dist_indices = dist_indices, M = M)

formula = z ~ normal(mean ~ 1 + x1, sd ~ 1 + x1, p ~ 1)
priors = list(mean = list((Intercept) ~ gamma(1, 1),
                          x1 ~ gamma(1, 1)),
              sd   = list((Intercept) ~ gamma(1, 1),
                          x1 ~ gamma(1, 1)),
              p    = list((Intercept) ~ beta(1, 1)))


strauss(formula = formula, priors = priors, data = data, chains = 1,
        control = list(adapt_delta = 0.99)) -> mods

library("magrittr")
rstan::extract(mods)$beta_positive[ , 1] %>% hist



## =============================================================================
## Simulation test 5: Skew normal with (1 + 1 + 1 + 1) parameters.
## Effect size distribution: Normal
## =============================================================================

set.seed(1337)
N = 100
M = sample(N, 20:80, replace = TRUE)
x1 = rexp(N, 5)
p = rbinom(N, 1, 0.7)
mean_ = 0.1 + 0.1*x1
sd_   = 0.1
thetas = rnorm(N, mean = mean_, sd = sd_)
lower = rep(1.96, N)
upper = rep(0, N)
z = rnorm(N, sqrt(M)*thetas, 1)*(1 - p) +
  truncnorm::rtruncnorm(N, mean = sqrt(M)*thetas, sd = 1, a = lower)*p

dist_indices = rep(3, N)
dist_indices[z < 1.96] = 7


data = data.frame(z = z, x1 = x1, lower = lower, upper = upper,
                  dist_indices = dist_indices, M = M)

formula = z ~ skew_normal(mean ~ 1 + x1, sd ~ 1, alpha ~ 1, p ~ 1)
priors = list(mean  = list((Intercept) ~ gamma(1, 1),
                          x1 ~ gamma(1, 1)),
              sd    = list((Intercept) ~ gamma(1, 1)),
              alpha = list((Intercept) ~ normal(0, 1)),
              p     = list((Intercept) ~ beta(1, 1)))


strauss(formula = formula, priors = priors, data = data, chains = 1,
        control = list(adapt_delta = 0.99)) -> mods

library("magrittr")
rstan::extract(mods)$beta_positive[ , 1] %>% hist
