## =============================================================================
## Example from "A Parsimonious Weight Function for Modeling Publication Bias"
## by Martyna Citkowicz and Jack Vevea.
## =============================================================================

baskerville = readRDS("data/baskerville.Rds")
N = nrow(baskerville)
baskerville$z = baskerville$effect_size*1/sqrt(baskerville$variance)
baskerville$M = 1/baskerville$variance
baskerville$lower = rep(qnorm(0.95), N)
baskerville$upper = rep(qnorm(0.95), N)
baskerville$dist_indices = rep(5, N) # 5: Mixture, normal with lower bound

#baskerville$lower[baskerville$z< qnorm(0.95)] = 0
baskerville$dist_indices[baskerville$z< qnorm(0.95)] = 10

formula = z ~ normal(mean ~ 1,
                     sd  ~ 1,
                     p ~ 1 )

priors = list(mean = list((Intercept) ~ normal(0, 1)),
              sd   = list((Intercept) ~ gamma(1, 20)),
              p    = list((Intercept) ~ beta(1, 1)))

straussR(formula = formula, data = baskerville, priors = priors, chains = 4,
         control = list(adapt_delta = 0.999)) ->
  baskerville_mixed


straussR(formula = z ~ fixed(mean ~ 1, p ~ 1),
         data = baskerville,
         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       p = list((Intercept) ~ beta(1, 1))),
         chains = 1,
         control = list(adapt_delta = 0.999)) ->
  baskerville_fixed



plot(baskerville$M, baskerville$effect_size, log = "x")
points(baskerville$M, colMeans(rstan::extract(baskerville_mixed)$thetas_unbounded), col = "red")


hist(rstan::extract(baskerville_mixed)$beta_unbounded)
hist(rstan::extract(baskerville_mixed)$beta_positive)
hist(rstan::extract(baskerville_mixed)$beta_unit)
hist(rstan::extract(baskerville_mixed)$thetas_unbounded)

hist(efnorm(rstan::extract(baskerville_mixed)$beta_unbounded,
       rstan::extract(baskerville_mixed)$beta_positive))
rstan::extract(baskerville_mixed)$beta_unbounded[, 1]
hist(rstan::extract(baskerville_mixed)$beta_unbounded[, 1])
hist(rstan::extract(baskerville_fixed)$beta_unbounded)
hist(rstan::extract(baskerville_fixed)$beta_unit)

## =============================================================================
## Posterior predictive, fixed
## =============================================================================

N = nrow(baskerville)
n = baskerville$M
p = rbinom(N, 1, sample(rstan::extract(baskerville_fixed)$beta_unit, N))
theta = sample(rstan::extract(baskerville_fixed)$beta_unbounded, N)
z = truncnorm::rtruncnorm(N, mean = sqrt(n)*theta, sd = 1, a = qnorm(0.95))
z = p*z + (1 - p)*rnorm(N, mean = sqrt(n)*theta, sd = 1)

plot(n, z/sqrt(n), log = "x", ylim = c(0, 2))
points(baskerville$M, baskerville$effect_size, pch = 20, col = "red")

## =============================================================================
## Posterior predictive, fixed
## =============================================================================

L = length(rstan::extract(baskerville_fixed)$beta_unbounded)
N = nrow(baskerville)
n = baskerville$M
p = rbinom(N, 1, sample(rstan::extract(baskerville_fixed)$beta_unit, N))
S = sample(L, N)

theta = rnorm(N, mean = rstan::extract(baskerville_fixed)$beta_unbounded[S],
              sd = rstan::extract(baskerville_fixed)$beta_positive[S])
z = truncnorm::rtruncnorm(N, mean = sqrt(n)*theta, sd = 1, a = qnorm(0.95))
z = p*z + (1 - p)*rnorm(N, mean = sqrt(n)*theta, sd = 1)

plot(n, z/sqrt(n), log = "x", ylim = c(0, 2))
points(baskerville$M, baskerville$effect_size, pch = 20, col = "red")