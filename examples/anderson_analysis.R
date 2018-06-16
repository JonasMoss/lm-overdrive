## Models with publication bias

straussR(formula = z ~ normal(mean ~ 1 + best,
                              sd  ~ 1,
                              p ~ 1),

         data = dplyr::filter(anderson_pb, outcome == "AggAff"),

         priors = list(mean = list((Intercept) ~ normal(0, 1),
                                   best ~ normal(0,1)),
                       sd   = list((Intercept) ~ gamma(1, 20)),
                       p    = list((Intercept) ~ beta(1, 1))),

         chains = 4,
         control = list(adapt_delta = 0.999)) ->
  AggAff_best_mixed_pb

straussR(formula = z ~ normal(mean ~ 1,
                              sd  ~ 1,
                              p ~ 1),

         data = dplyr::filter(anderson_pb, outcome == "AggAff"),

         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       sd   = list((Intercept) ~ gamma(1, 20)),
                       p    = list((Intercept) ~ beta(1, 1))),

         chains = 4,
         control = list(adapt_delta = 0.999)) ->
  AggAff_mixed_pb

straussR(formula = z ~ fixed(mean ~ 1 + best,
                             p ~ 1),

         data = dplyr::filter(anderson_pb, outcome == "AggAff"),

         priors = list(mean = list((Intercept) ~ normal(0, 1),
                                   best ~ normal(0,1)),
                       p    = list((Intercept) ~ beta(1, 1))),

         chains = 4,
         control = list(adapt_delta = 0.999)) ->
  AggAff_best_fixed_pb

straussR(formula = z ~ fixed(mean ~ 1,
                             p ~ 1),

         data = dplyr::filter(anderson_pb, outcome == "AggAff"),

         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       p    = list((Intercept) ~ beta(1, 1))),

         chains = 4,
         control = list(adapt_delta = 0.999)) ->
  AggAff_fixed_pb

# Models without publication bias

straussR(formula = z ~ normal(mean ~ 1 + best,
                              sd  ~ 1),

         data = dplyr::filter(anderson_npb, outcome == "AggAff"),

         priors = list(mean = list((Intercept) ~ normal(0, 1),
                                   best ~ normal(0,1)),
                       sd   = list((Intercept) ~ gamma(1, 20))),

         chains = 4,
         control = list(adapt_delta = 0.999)) ->
  AggAff_best_mixed

straussR(formula = z ~ normal(mean ~ 1,
                              sd  ~ 1),

         data = dplyr::filter(anderson_npb, outcome == "AggAff"),

         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       sd   = list((Intercept) ~ gamma(1, 20))),

         chains = 4,
         control = list(adapt_delta = 0.999)) ->
  AggAff_mixed

straussR(formula = z ~ fixed(mean ~ 1 + best),

         data = dplyr::filter(anderson_npb, outcome == "AggAff"),

         priors = list(mean = list((Intercept) ~ normal(0, 1),
                                   best ~ normal(0,1))),

         chains = 4,
         control = list(adapt_delta = 0.999)) ->
  AggAff_best_fixed

straussR(formula = z ~ fixed(mean ~ 1),

         data = dplyr::filter(anderson_npb, outcome == "AggAff"),

         priors = list(mean = list((Intercept) ~ normal(0, 1))),

         chains = 4,
         control = list(adapt_delta = 0.999)) ->
  AggAff_fixed


## =======================================================
## Posteriors
## =======================================================

theta0_pb = rstan::extract(AggAff_mixed_pb$stan_object)$beta_unbounded
theta0_npb = rstan::extract(AggAff_mixed$stan_object)$beta_unbounded
hist(x = theta0_npb, freq = FALSE, breaks = 50,
     col = rgb(0, 0, 1, 1/4), xlim = c(0, 0.2),
     main = expression(paste("Posteriors for ", theta[0])),
     xlab = expression(theta),
     ylab = "Density")
hist(x = theta0_pb, freq = FALSE, breaks = 50,
     col = rgb(1, 0, 1, 1/4), add = TRUE)
legend("topleft", col = c(rgb(1, 0, 1, 1/4), rgb(0, 0, 1, 1/4)),
       legend = c(paste0("With correction (mean = ",
                         round(mean(theta0_pb), 3), ")"),
                  paste0("Without correction (mean = ",
                         round(mean(theta0_npb), 3), ")")),
       lty = c(1, 1), lwd = c(2, 2), bty = "n")

## ============================================================
## Posterior predictive
## ============================================================
dat = dplyr::filter(anderson_pb, outcome == "AggAff")
plot(dat$M, dat$z/sqrt(dat$M), log = "x", pch = 20)
lines(sort(dat$M), 1.64/sqrt(sort(dat$M)))

#npb
set.seed(5)
thetas_npb = sample(rstan::extract(AggAff_mixed$stan_object)$beta_unbounded, 75)
sigmas_npb = sample(rstan::extract(AggAff_mixed$stan_object)$beta_positive, 75)
theta0s_npb = rnorm(75, thetas_npb, sigmas_npb)
vals_npb = rnorm(75, theta0s_npb, 1/sqrt(dat$M))
points(dat$M, vals_npb, col = "blue", pch = 20)

# bp
thetas_pb = sample(rstan::extract(AggAff_mixed_pb$stan_object)$beta_unbounded, 75)
sigmas_pb = sample(rstan::extract(AggAff_mixed_pb$stan_object)$beta_positive, 75)
theta0s_pb = rnorm(75, thetas_bp, sigmas_bp)
p_pb = sample(rstan::extract(AggAff_mixed_pb$stan_object)$beta_unit, 75)
ind_pb = rbinom(75, prob = p_pb, size = 1)
vals_pb = (1 - ind_pb)*rnorm(75, theta0s_bp, 1/sqrt(dat$M)) +
          ind_pb*truncnorm::rtruncnorm(75, mean = theta0s_bp, sd = 1/sqrt(dat$M), a = 1.64/sqrt(dat$M))
points(dat$M, vals_pb, col = "red", pch = 20)

replicate(1000, {
  thetas_npb = sample(rstan::extract(AggAff_mixed$stan_object)$beta_unbounded, 75)
  sigmas_npb = sample(rstan::extract(AggAff_mixed$stan_object)$beta_positive, 75)
  theta0s_npb = rnorm(75, thetas_npb, sigmas_npb)
  vals_npb = rnorm(75, theta0s_npb, 1/sqrt(dat$M))
  thetas_pb = sample(rstan::extract(AggAff_mixed_pb$stan_object)$beta_unbounded, 75)
  sigmas_pb = sample(rstan::extract(AggAff_mixed_pb$stan_object)$beta_positive, 75)
  theta0s_pb = rnorm(75, thetas_bp, sigmas_bp)
  p_pb = sample(rstan::extract(AggAff_mixed_pb$stan_object)$beta_unit, 75)
  ind_pb = rbinom(75, prob = p_pb, size = 1)
  vals_pb = (1 - ind_pb)*rnorm(75, theta0s_bp, 1/sqrt(dat$M)) +
    ind_pb*truncnorm::rtruncnorm(75, mean = theta0s_bp, sd = 1/sqrt(dat$M), a = 1.64/sqrt(dat$M))
  c(npb = mean(abs(vals_npb - dat$z/sqrt(dat$M))^2),
    pb = mean(abs(vals_pb - dat$z/sqrt(dat$M)))^2)
}) -> A