## =============================================================================
## Illustration of p-hacking
## =============================================================================
correlation = cor(log(sqrt(data$M)), log(data$z/sqrt(data$M)))
plot(sqrt(data$M), data$z/sqrt(data$M), ylim = c(0.1, 1), log = "xy",
     xlab = expression(sqrt(n)), ylab = "d", bty = "l", pch = 20, col = "black",
     cex = 1.6, main = "Estimated effect sizes from Motyl et al. (2017)",
     sub = paste0("Correlation:", signif(correlation, digits = 2)))
lines(sort(sqrt(data$M)), 1.96/sort(sqrt(data$M)),
      col = "red", lty = 2, lwd = 2)
legend("topright", c("Estimated effect", expression(1.96/sqrt(n))),
       col = c("black", "red"), bty = "n", pch = c(20, NA), lty = c(NA, 2),
       lwd = c(NA, 2))


data = motyl_data
formula = z ~ gamma(mean ~ 1 + M, sd ~ 1, p ~ 1)
priors = list(mean = list((Intercept) ~ gamma(1, 1),
                          M ~ gamma(1, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p    = list((Intercept) ~ beta(1, 1)))

straussR(formula = formula, data = motyl_data, priors = priors, chains = 1,
        control = list(adapt_delta = 0.99)) -> mods


rstan::extract(mods)$beta_positive[ , 1] %>% hist
rstan::extract(mods)$beta_positive[ , 2] %>% hist

plot(motyl_data$d, colMeans(rstan::extract(mods)$thetas_positive))

plot(log(motyl_data$M), log(colMeans(rstan::extract(mods)$thetas_positive)))
cor(log(motyl_data$M), log(colMeans(rstan::extract(mods)$thetas_positive)))
plot(log(motyl_data$M), log(motyl_data$d))
cor(log(motyl_data$M), log(motyl_data$d))


plot(log(colMeans(rstan::extract(mods)$thetas_positive)), log(motyl_data$M))


## =============================================================================
## Take 2: Logarithms and variying sd.
## =============================================================================

formula = z ~ gamma(mean ~ 1, log(sd) ~ 1 + M_scaled, log(p) ~ 1 + M_scaled)
priors = list(mean   = list((Intercept) ~ gamma(1, 1)),
              sd = list((Intercept) ~ normal(0, 1),
                        M_scaled ~ normal(0, 1)),
              p    = list((Intercept) ~ normal(0, 1),
                          M_scaled ~ normal(0, 1)))

straussR(formula = formula, data = motyl_data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.99)) -> mods_vsd


rstan::extract(mods)$beta_positive[ , 1] %>% hist
rstan::extract(mods)$beta_positive[ , 2] %>% hist

plot(motyl_data$d, colMeans(rstan::extract(mods)$thetas_positive),
     xlim = c(0, 1), ylim = c(0, 1))

plot(log(motyl_data$M), log(colMeans(rstan::extract(mods)$thetas_positive)))
cor(log(motyl_data$M), log(colMeans(rstan::extract(mods)$thetas_positive)))
plot(log(motyl_data$M), log(motyl_data$d))
cor(log(motyl_data$M), log(motyl_data$d))

## =============================================================================
## Take 3: Logarithms: Logit transform of p wrt M_scaled.
## =============================================================================

formula = z ~ gamma(mean ~ 1, sd ~ 1, logit(p) ~ 1 + M_scaled)
priors = list(mean = list((Intercept) ~ gamma(1, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p    = list((Intercept) ~ normal(0, 1),
                          M_scaled ~ normal(0, 1)))

straussR(formula = formula, data = motyl_data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.99)) -> mods_log_csd


rstan::extract(mods_log_csd)$beta_positive[ , 1] %>% hist
rstan::extract(mods_log_csd)$beta_positive[ , 2] %>% hist
mean_thetas = colMeans(rstan::extract(mods_log_csd)$thetas_positive)

plot(motyl_data$d, mean_thetas,
     xlim = c(0, 1), ylim = c(0, 1))



plot(motyl_data$M, mean_thetas, col = "blue", ylim = c(0, 0.7), pch = 20,
     cex = 2, bty = "l", xlab = "Study size", ylab = expression(theta))

points(motyl_data$M, motyl_data$d, col = "red", pch = 20, cex = 2)
legend("topright", c("Original", "Corrected"), col = c("red", "blue"),
       pch = c(20, 20), cex = c(2, 2), bty = "n")

## =============================================================================
## Take 3: Most simple model.
## =============================================================================

formula = z ~ gamma(mean ~ 1,
                    sd   ~ 1,
                    p    ~ 1)

priors = list(mean = list((Intercept) ~ gamma(1, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p    = list((Intercept) ~ gamma(1, 1)))

straussR(formula = formula, data = motyl_data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.99)) -> mods_simple_gamma


rstan::extract(mods_simple_gamma)$beta_positive[ , 1] %>% hist
rstan::extract(mods_simple_gamma)$beta_positive[ , 2] %>% hist

thetas = rstan::extract(mods_simple_gamma)$thetas_positive
mean_thetas = colMeans(thetas)
mean_thetas = apply(thetas, 2, median)
sd_thetas = apply(thetas, 2, sd)

thetas_10 = apply(thetas, 2, function(x) quantile(x, probs = c(0.1)))
thetas_50 = apply(thetas, 2, function(x) quantile(x, probs = c(0.5)))
thetas_90 = apply(thetas, 2, function(x) quantile(x, probs = c(0.9)))

hist(power_distribution(mods_simple_gamma, n = motyl_data$M), freq = FALSE,
     breaks = 100)

plot(motyl_data$d, mean_thetas,
     xlim = c(0, 1), ylim = c(0, 1))


plot(motyl_data$M, thetas_50, col = "blue", ylim = c(0, 0.7), pch = 20,
     cex = 1.5, bty = "l", xlab = "Study size", ylab = expression(theta))
points(motyl_data$M, thetas_10, cex = 0.7, col = "blue")
points(motyl_data$M, thetas_90, cex = 0.7, col = "blue")
abline(h = 0, col = "red", lty = 3)

plot(motyl_data$M, mean_thetas, col = "blue", ylim = c(0, 0.7), pch = 20,
     cex = 2, bty = "l", xlab = "Study size", ylab = expression(theta))

points(motyl_data$M, motyl_data$d, col = "red", pch = 20, cex = 2)
legend("topright", c("Original", "Corrected"), col = c("red", "blue"),
       pch = c(20, 20), cex = c(2, 2), bty = "n")

## =============================================================================
## Take 4: Logarithms and varying mean.
## =============================================================================

formula = z ~ gamma(log(mean) ~ 1 + M_scaled,
                    log(sd) ~ 1,
                    log(p) ~ 1 + M_scaled)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          M_scaled ~ normal(0, 1)),
              sd   = list((Intercept) ~ normal(0, 1)),
              p    = list((Intercept) ~ normal(0, 1),
                          M_scaled ~ normal(0, 1)))

straussR(formula = formula, data = motyl_data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.99)) -> mods_log_vm


mean_thetas = colMeans(rstan::extract(mods_log_vm)$thetas_positive)

plot(motyl_data$d, mean_thetas,
     xlim = c(0, 1), ylim = c(0, 1))

plot(motyl_data$M, mean_thetas, log = "xy")
points(motyl_data$M, motyl_data$d, col = "red")
cor(log(motyl_data$M), log(mean_thetas))
cor(log(motyl_data$M), log(motyl_data$d))

lplot(sort(motyl_data$M), exp(-1.64 - 0.27*sort(motyl_data$M_scaled)))

## =============================================================================
## Take 5: Logarithms and varying mean, sd.
## =============================================================================

formula = z ~ gamma(log(mean) ~ 1 + M_scaled,
                    log(sd) ~ 1 + M_scaled,
                    log(p) ~ 1 + M_scaled)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          M_scaled ~ normal(0, 1)),
              sd   = list((Intercept) ~ normal(0, 1),
                          M_scaled ~ normal(0, 1)),
              p    = list((Intercept) ~ normal(0, 1),
                          M_scaled ~ normal(0, 1)))

straussR(formula = formula, data = motyl_data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.99)) -> mods_log_vms


lplot(sort(motyl_data$M), exp(-1.64 - 0.27*sort(motyl_data$M_scaled)))

## =============================================================================
## Posterior predictive distributions
## =============================================================================

x = seq(0, 1.5, by = 0.01)
predictive_gamma           = dppredictive(x, fit_gamma_mixed2)
predictive_gamma_classical = dppredictive(x, fit_gamma_classical2)
predictive_ig              = dppredictive(x, fit_inverse_gaussian_mixed)
predictive_ig_classical    = dppredictive(x, fit_inverse_gaussian_classical)

lplot(x, predictive_gamma, xlab = expression(theta), ylab = "Density",
      main = "Posterior predictive distribution", lwd = 2,
      ylim = c(0, 3.3))
lines(x, predictive_gamma_classical, col = "blue", lwd = 2)
lines(x, predictive_ig, col = "red", lwd = 2)
lines(x, predictive_ig_classical, col = "green", lwd = 2)
legend("topright", legend = c("Gamma, mixed",
                              "Gamma, classical",
                              "IG, mixed",
                              "IG, classical"),
       col = c("black", "blue", "red", "green"),
       lwd = rep(2, 4), lty = rep(1, 4), bty = "n")

## =============================================================================
## Power histograms
## =============================================================================

power_distribution(fit_gamma_mixed) %>%
  hist(breaks = 200, freq = FALSE)
power_distribution(fit_gamma_mixed) %>% mean

power_distribution(fit_gamma_classical) %>%
  hist(breaks = 200, freq = FALSE)
power_distribution(fit_gamma_classical) %>% mean

power_distribution(fit_inverse_gaussian_mixed) %>%
  hist(breaks = 200, freq = FALSE)
power_distribution(fit_inverse_gaussian_mixed) %>% mean

power_distribution(fit_inverse_gaussian_classical) %>%
  hist(breaks = 200, freq = FALSE)
power_distribution(fit_inverse_gaussian_classical) %>% mean








# Comparison of results:

x = seq(0, 1.5, by = 0.01)
pred = dppredictive(x, fit_gamma_0)
lplot(x,pred, xlab = expression(theta),
      ylab = "Density", main = "Posterior predictive distribution")
lines(x, dppredictive(x, fit_gamma_1), col = "blue")
lines(x, dppredictive(x, fit_gamma_2), col = "red")
lines(x, dppredictive(x, fit_gamma_3), col = "green")
legend("topright", c("Mixed", "p-curve", "Classical", "Reverse p-curve"),
       col = c("black", "blue", "red", "green"), lty = rep(1, 4), bty = "n")
grid()



x = seq(0, 2, by = 0.01)
predictive_gamma = dppredictive(x, fit_gamma_0)
predictive_ig           = dppredictive(x, fit_ig_0)
predictive_gamma_classical = dppredictive(x, fit_gamma_3)

lplot(x, predictive_gamma, xlab = expression(theta), ylab = "Density",
      main = "Posterior predictive distribution", lwd = 2)
lines(x, predictive_gamma_classical, col = "blue", lwd = 2)


lines(kdensity::kdensity(motyl_data$d, start = "gamma", adjust = 2),
      lty = 2.5)


power_distribution(fit_gamma_0) %>%
  hist(breaks = 200, freq = FALSE)


power_distribution(fit_gamma_classical) %>%
  hist(breaks = 200, freq = FALSE)

hist(extract(fit_gamma_0$fit)$p_)
hist(extract(fit_gamma_0$fit)$mean_)
hist(extract(fit_gamma_0$fit)$sd_)

hist(extract(fit_ig_0$fit)$p_)
hist(extract(fit_ig_0$fit)$mean_)
hist(extract(fit_ig_0$fit)$sd_)


hist(c(extract(fit_gamma_0$fit)$thetas_abs_b,
       extract(fit_gamma_0$fit)$thetas_abs_g), freq = FALSE, breaks = 200)

hist(c(extract(fit_ig_0$fit)$thetas_abs_b,
       extract(fit_ig_0$fit)$thetas_abs_g), freq = FALSE, breaks = 200)


plot(fit_gamma_0$stan_data$d_b,
     colMeans(extract(fit_gamma_0$fit)$thetas_b),
     ylim = c(0, 1.6), xlim = c(0, 1.6))
abline(a = 0, b = 1)


plot(fit_gamma_mixed$stan_data$n_abs_b,
     colMeans(extract(fit_gamma_mixed$fit)$thetas_abs_b), log = "xy",
     xlab = "Sample size", ylab = "Estimated effect size",
     bty = "l", pch = 20)

plot(fit_gamma_classical$stan_data$n_abs_b,
     colMeans(extract(fit_gamma_classical$fit)$thetas_abs_b), log = "xy",
     xlab = "Sample size", ylab = "Estimated effect size",
     bty = "l", pch = 20, col = "blue")

plot(fit_gamma_mixed$stan_data$n_abs_b,
     fit_gamma_mixed$stan_data$d_abs_b, log = "xy",
     xlab = "Sample size", ylab = "Estimated effect size",
     bty = "l", pch = 20, col = "red")


plot(colMeans(extract(fit_gamma_mixed$fit)$thetas_abs_b),
     fit_gamma_mixed$stan_data$d_abs_b)

lm(log(colMeans(extract(fit_gamma_mixed$fit)$thetas_abs_b)) ~
   log(fit_gamma_mixed$stan_data$n_abs_b)) %>% summary

lm(log(colMeans(extract(fit_gamma_mixed$fit)$thetas_abs_b)) ~
   log(fit_gamma_mixed$stan_data$d_abs_b)) %>% summary

cor(log(fit_gamma_mixed$stan_data$n_abs_b),
    log(colMeans(extract(fit_gamma_mixed$fit)$thetas_abs_b)))

cor(log(fit_gamma_mixed$stan_data$n_abs_b),
    log(fit_gamma_mixed$stan_data$d_abs_b))

hist(colMeans(extract(fit_gamma_0$fit)$thetas_b), freq = FALSE)

## Power and n
N = length(motyl_data$z)

powers = sapply(1:N, function(i) {
  mean(power_distribution(fit_gamma_0, indices = i))
})

object = fit_gamma_0
thetas_b = rstan::extract(object$fit)$thetas_b
thetas_g = rstan::extract(object$fit)$thetas_g
thetas = cbind(thetas_b, thetas_g)
thetas[, object$stan_data$indices_b] = thetas_b
thetas[, object$stan_data$indices_g] = thetas_g
theta_means = colMeans(thetas)

i = sample(1:length(motyl_data$d), 1)
hist(thetas[ , i], freq = FALSE)
abline(v = motyl_data$d[i], col = "red", lwd = 2)
abline(v = theta_means[i], col = "blue", lwd = 2)



plot(theta_means, powers)

plot(motyl_data$n, powers)

plot(motyl_data$z, powers)

new_df = cbind(motyl_data$n, theta_means, motyl_data$d)
colnames(new_df) = c("n", "theta", "d")
new_df = as.data.frame(new_df)
new_df = dplyr::filter(new_df, n < 200)

plot(new_df$n, new_df$theta)
ggplot2::ggplot(data = new_df, aes(x = n, y = theta)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm")

ggplot2::ggplot(data = new_df, aes(x = n, y = d)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm")

lm(theta ~ n, data = new_df)


plot(log(motyl_data$n), theta_means)