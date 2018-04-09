formula = mpg ~ cyl + disp + hp + drat
data = mtcars
priors = list((Intercept) ~ gamma(2, 2),
              cyl         ~ student_t(2, 1, 3),
              hp          ~ exp_mod_normal(0, 1, 3),
              disp        ~ weibull(2, 2),
              drat        ~ beta(3, 3))

priors = list((Intercept) ~ beta(3, 3),
              cyl         ~ beta(3, 3),
              hp          ~ beta(3, 3),
              disp        ~ beta(3, 3),
              drat        ~ beta(3, 3))

priors = list((Intercept) ~ normal(0, 1),
              cyl         ~ normal(0, 1),
              hp          ~ normal(0, 1),
              disp        ~ normal(0, 1),
              drat        ~ normal(0, 1))

stanlm(formula, data, priors,
       link = "identity", family = "normal") -> mod_normal

stanlm(formula, data, priors,
       link = "identity", family = "gumbel") -> mod_gumbel

stanlm(formula, data, priors,
       link = "identity", family = "skew_normal") -> mod_skew_normal

rstan::extract(mod_skew_normal)$unbounded_parameters[, 1] %>% hist(breaks = 50)