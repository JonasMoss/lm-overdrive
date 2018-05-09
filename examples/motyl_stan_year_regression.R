data = motyl_data

formula = z ~ normal(mean ~ 1,
                     sd ~ 1,
                     probit(p) ~ 1 + n)

priors = list(mean = list((Intercept) ~ normal(0, 1)),
              sd = list((Intercept) ~ gamma(3, 1)),
              p = list((Intercept) ~ normal(0, 1),
                       n ~ gumbel(0, 1)))

straussR(formula = formula,
         data = data,
         priors = priors,
         chains = 1,
         control = list(adapt_delta = 0.999)) ->
  x



thetas = rstan::extract(motyl_year)$thetas_positive

mean_2014 = rstan::extract(motyl_year)$beta_unbounded[, 1]
mean_2003 = rstan::extract(motyl_year)$beta_unbounded[, 2]
mean_2004 = rstan::extract(motyl_year)$beta_unbounded[, 3]
mean_2013 = rstan::extract(motyl_year)$beta_unbounded[, 4]

p2014 = rstan::extract(motyl_year)$beta_unbounded[, 5]
p2003 = rstan::extract(motyl_year)$beta_unbounded[, 6]
p2004 = rstan::extract(motyl_year)$beta_unbounded[, 7]
p2013 = rstan::extract(motyl_year)$beta_unbounded[, 8]

mean_thetas = colMeans(thetas)
mean_thetas = apply(thetas, 2, median)
sd_thetas = apply(thetas, 2, sd)

thetas_10 = apply(thetas, 2, function(x) quantile(x, probs = c(0.05)))
thetas_50 = apply(thetas, 2, function(x) quantile(x, probs = c(0.5)))
thetas_90 = apply(thetas, 2, function(x) quantile(x, probs = c(0.95)))

orders = order(motyl_data$M)
lplot(motyl_data$M[orders], thetas_50[orders], ylim = c(-0.01, 1), col = "blue")
lines(motyl_data$M[orders], thetas_10[orders], col = "red")
lines(motyl_data$M[orders], thetas_90[orders], col = "red")
points(motyl_data$M[orders], motyl_data$d[orders])

hist(power_distribution(motyl_year, n = motyl_data$M), freq = FALSE,
     breaks = 100)

hist(exp(p2014)/(1 + exp(p2014)))
hist(exp(p2014 + p2003)/(1 + exp(p2014 + p2003)))
hist(exp(p2014 + p2013)/(1 + exp(p2014 + p2013)))
hist(exp(p2014 + p2004)/(1 + exp(p2014 + p2004)))

hist(exp(mean_2014)/(1 + exp(mean_2014)))
hist(exp(mean_2014 + mean_2003)/(1 + exp(mean_2014 + mean_2003)))
hist(exp(mean_2014 + mean_2013)/(1 + exp(mean_2014 + mean_2013)))
hist(exp(mean_2014 + mean_2004)/(1 + exp(mean_2014 + mean_2004)))

thetas[, data$year == 2004] %>% colMeans

## =============================================================================
## With more sd.
## =============================================================================

formula = z ~ fnormal(log(mean) ~ 1 + year_factor,
                      sd        ~ 1 + year_factor,
                      probit(p) ~ 1 + year_factor + M_scaled)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          year_factor ~ normal(0, 1)),
              sd   = list((Intercept) ~ gamma(1, 1),
                          year_factor ~ gamma(1, 1)),
              p    = list((Intercept) ~ normal(0, 1),
                          year_factor ~ normal(0, 1),
                          M_scaled    ~ normal(0, 1)))

N = motyl_data

straussR(formula = formula, data = motyl_data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999)) ->
  motyl_year2


thetas = rstan::extract(motyl_year2)$thetas_positive

mean_2014 = exp(rstan::extract(motyl_year2)$beta_unbounded[, 1])
mean_2003 = exp(rstan::extract(motyl_year2)$beta_unbounded[, 2])*mean_2014
mean_2004 = exp(rstan::extract(motyl_year2)$beta_unbounded[, 3])*mean_2014
mean_2013 = exp(rstan::extract(motyl_year2)$beta_unbounded[, 4])*mean_2014

sd_2014 = rstan::extract(motyl_year2)$beta_positive[, 1]
sd_2003 = rstan::extract(motyl_year2)$beta_positive[, 2] + sd_2014
sd_2004 = rstan::extract(motyl_year2)$beta_positive[, 3] + sd_2014
sd_2013 = rstan::extract(motyl_year2)$beta_positive[, 4] + sd_2014

means_2003 = efnorm(mean = mean_2003,
                    sd   = sd_2003)
means_2004 = efnorm(mean = mean_2004,
                    sd   = sd_2004)
means_2013 = efnorm(mean = mean_2013,
                    sd   = sd_2013)
means_2014 = efnorm(mean = mean_2014,
                    sd   = sd_2014)


year_N = table(motyl_data$year)

par(mfrow = c(2, 2))
hist(means_2003, main = paste0("Means of 2003 (n = ", year_N[1], " )"),
     xlab = expression(theta), breaks = 10, freq = FALSE)
hist(means_2004, main = paste0("Means of 2004 (n = ", year_N[2], " )"),
     xlab = expression(theta), breaks = 10, freq = FALSE)
hist(means_2013, main = paste0("Means of 2013 (n = ", year_N[3], " )"),
     xlab = expression(theta), breaks = 10, freq = FALSE)
hist(means_2014, main = paste0("Means of 2014 (n = ", year_N[4], " )"),
     xlab = expression(theta), breaks = 10, freq = FALSE)

dev.off()
power_distribution(motyl_year2, n = motyl_data$M) %>%
  hist(breaks = 200, freq = FALSE, main = "Power")

p2014 = rstan::extract(motyl_year2)$beta_unbounded[, 5]
p2003 = rstan::extract(motyl_year2)$beta_unbounded[, 6]
p2004 = rstan::extract(motyl_year2)$beta_unbounded[, 7]
p2013 = rstan::extract(motyl_year2)$beta_unbounded[, 8]

par(mfrow = c(2, 2))
hist(exp(p2014 + p2003)/(1 + exp(p2014 + p2003)),
     main = paste0("p 2003 (n = ", year_N[2], " )"),
     xlab = expression(theta), breaks = 10, freq = FALSE)
hist(exp(p2014 + p2004)/(1 + exp(p2014 + p2004)),
     main = paste0("p 2004 (n = ", year_N[4], " )"),
     xlab = expression(theta), breaks = 10, freq = FALSE)
hist(exp(p2014 + p2013)/(1 + exp(p2014 + p2013)),
     main = paste0("p 2013 (n = ", year_N[3], " )"),
     xlab = expression(theta), breaks = 10, freq = FALSE)
hist(exp(p2014)/(1 + exp(p2014)),
     main = paste0("p 2014 (n = ", year_N[1], " )"),
     xlab = expression(theta), breaks = 10, freq = FALSE)


mean_thetas = colMeans(thetas)
mean_thetas = apply(thetas, 2, median)
sd_thetas = apply(thetas, 2, sd)

thetas_10 = apply(thetas, 2, function(x) quantile(x, probs = c(0.1)))
thetas_50 = apply(thetas, 2, function(x) quantile(x, probs = c(0.5)))
thetas_90 = apply(thetas, 2, function(x) quantile(x, probs = c(0.9)))

hist(power_distribution(motyl_year2, n = motyl_data$M), freq = FALSE,
     breaks = 100)





hist(exp(mean_2014))
hist(exp(mean_2014 + mean_2003))
hist(exp(mean_2014 + mean_2013))
hist(exp(mean_2014 + mean_2004))

thetas[, data$year == 2004] %>% colMeans
