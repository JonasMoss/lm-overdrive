formula = z ~ fnormal(log(mean) ~ 1 + year_factor,
                    sd        ~ 1,
                    logit(p)  ~ 1 + year_factor + M_scaled)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          year_factor ~ normal(0, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p    = list((Intercept) ~ normal(0, 1),
                          year_factor ~ normal(0, 1),
                          M_scaled    ~ normal(0, 1)))

N = motyl_data

straussR(formula = formula, data = motyl_data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999), init = function(chain_id = 1) {
           no_unbounded = sum(sdata$no_unbounded)
           no_positive = sum(sdata$no_positive)
           no_unit = sum(sdata$no_unit)
           init = function(chain_id = 1) {
             list(thetas_unbounded = rep(0.1, 9),
                  thetas_positive  = rep(0.1, 1),
                  beta_unbounded   = rnorm(9, 0, 0.1),
                  beta_positive    = rgamma(1, 1, 1))
           }
           dots$init = init
         }) ->
  motyl_year2


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

thetas_10 = apply(thetas, 2, function(x) quantile(x, probs = c(0.1)))
thetas_50 = apply(thetas, 2, function(x) quantile(x, probs = c(0.5)))
thetas_90 = apply(thetas, 2, function(x) quantile(x, probs = c(0.9)))

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