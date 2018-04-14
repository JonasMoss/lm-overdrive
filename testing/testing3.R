library("rstan")

formula = mpg ~ gumbel(mean ~ disp + wt,
                       inverse_squared(sd) ~ cyl)
p_formula = p ~ cyl

mean_priors = list(
  (Intercept) ~ normal(0, 1),
  disp ~ normal(0, 1),
  wt ~ normal(0, 1))

mean_priors = list(
  (Intercept) ~ gamma(1, 1),
  disp ~ skew_normal(xi = 1, omega = 2, alpha = 3),
  wt ~ student_t(nu = 1, mu = 4, alpha = 4))

sd_priors = list(
  (Intercept) ~ normal(0, 1),
  cyl ~ skew_normal(xi = 1, omega = 2, alpha = 3))

p_priors = list(
  (Intercept) ~ normal(0, 1),
  cyl ~ normal(0, 1))




mtscars = scale(mtcars)

stanlm(formula, data = as.data.frame(mtscars),
       p_formula,
       mean_priors,
       sd_priors,
       p_priors) ->
  mod



priors = list(mean_priors, sd_priors)






formula = mpg ~ normal(mean ~ disp + wt,
                         sd ~ 1)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          disp       ~ normal(0, 1),
                          wt         ~ normal(0, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)))





