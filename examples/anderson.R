hilgard = readr::read_table2("data/hilgard.txt")
hilgard = readxl::read_xls("data/hilgard.xls")

# The z-statistic is fisher's transform multiplied by
# n/sqrt(n + 3)
n = hilgard$`Sample size`
z_transform = hilgard$`Fisher's Z`
z = z_transform*n/sqrt(n + 3)
N = nrow(hilgard)
hilgard$t = hilgard$Correlation*sqrt((n-2)/(1 - hilgard$Correlation^2))

data = data.frame(z = z,
                  M = n,
                  lower = rep(qnorm(0.95), N),
                  upper = rep(0, N),
                  dist_indices = rep(5, N),
                  outcome = as.factor(hilgard$Outcome),
                  best = as.factor(hilgard$`Best?`))







































data$dist_indices[data$z < qnorm(0.95)] = 10

formula = z ~ normal(mean ~ 1 + outcome + best,
                     sd  ~ 1,
                     p ~ 1)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          outcome ~ normal(0,1),
                          best ~ normal(0,1)),
              sd   = list((Intercept) ~ gamma(1, 20)),
              p    = list((Intercept) ~ beta(1, 1)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999)) ->
  aggression_mixed

thetas = rstan::extract(aggression_mixed$stan_object)$thetas
hist(thetas)
hist(rstan::extract(aggression_mixed$stan_object)$beta_unit)

## =============================================================================
## Specific outcomes
## =============================================================================

data = dplyr::filter(data, outcome == "AggAff")

formula = z ~ normal(mean ~ 1 + best,
                     sd  ~ 1,
                     p ~ 1)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          best ~ normal(0,1)),
              sd   = list((Intercept) ~ gamma(1, 20)),
              p    = list((Intercept) ~ beta(1, 1)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999)) ->
  aggression_mixed_isolated

thetas = rstan::extract(aggression_mixed_isolated$stan_object)$thetas
hist(thetas)
hist(rstan::extract(aggression_mixed_isolated$stan_object)$beta_unit)


## =============================================================================
## Specific outcomes, good
## =============================================================================

data = dplyr::filter(data, outcome == "AggAff")
data$dist_indices = 20
formula = z ~ normal(mean ~ 1 + best,
                     sd  ~ 1)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          best ~ normal(0,1)),
              sd   = list((Intercept) ~ gamma(1, 1)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999)) ->
  aggression_mixed_isolated_good

thetas = rstan::extract(aggression_mixed_isolated_good$stan_object)$thetas
hist(thetas)



plot(data$M, data$z/sqrt(data$M), col = data$best, pch = 20, bty = "l",
     ylab = "Fisher Z-transform", xlab = "n", log = "x")
lines(sort(data$M), 1.64/sqrt(sort(data$M)))
legend("topright", col = c("black", "red"),
       legend = c("Best practices", "Not best practices"),
       pch = c(20, 20), bty = "n")


points(data$M, sqrt(data$M)*colMeans(rstan::extract(aggression_mixed_isolated$stan_object)$thetas)/sqrt(data$M),
       col = data$best, pch = 15)
points(data$M, sqrt(data$M)*colMeans(rstan::extract(aggression_mixed_isolated_good$stan_object)$thetas)/sqrt(data$M),
       col = data$best, pch = 17)