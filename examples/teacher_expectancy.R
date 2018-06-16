teacher = readr::read_csv("data/teacherexpectancy.txt")

cutoff = qnorm(0.95)
data = data.frame(z = teacher$d/teacher$sd,
                  d = teacher$d,
                  sd = teacher$sd,
                  M = 1/teacher$sd^2,
                  lower = rep(cutoff, nrow(teacher)),
                  upper = rep(0, nrow(teacher)),
                  dist_indices = rep(5, nrow(teacher)),
                  type = as.factor(teacher$weaks < 2))

data$dist_indices[data$z < cutoff] = 10



## =============================================================================
## First plot
## =============================================================================

x = seq(0, max(data$M) + 1, by = 0.1)
plot(data$M, data$d, col = data$type, pch = 20, xlab = "n",
     ylab = "Effect size", bty = "l", log = "x", ylim = c(-0.6, 1.4))
abline(h = 0, col = "grey", lty = 2)
lines(x, cutoff/sqrt(x))
lines(x, -cutoff/sqrt(x))




## =============================================================================
## With p-hacking
## =============================================================================

formula = z ~ normal(mean ~ 1,
                     sd  ~ 1,
                     p ~ 1)

priors = list(mean = list((Intercept) ~ normal(0, 1)),
              sd   = list((Intercept) ~ gamma(1, 20)),
              p    = list((Intercept) ~ beta(1, 1)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999)) ->
  teacher_mixed


## =============================================================================
## Without p-hacking
## =============================================================================

data$dist_indices = 20
formula = z ~ normal(mean ~ 1, sd  ~ 1)

priors = list(mean = list((Intercept) ~ normal(0, 1)),
              sd   = list((Intercept) ~ gamma(1, 20)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999)) ->
  teacher_good



## =============================================================================
## With p-hacking (2)
## =============================================================================

formula = z ~ normal(mean ~ 1 + type,
                     sd  ~ 1,
                     p ~ 1)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          type ~ normal(0, 1)),
              sd   = list((Intercept) ~ gamma(1, 20)),
              p    = list((Intercept) ~ beta(1, 1)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999)) ->
  teacher_mixed_2


## =============================================================================
## Without p-hacking (2)
## =============================================================================

data$dist_indices = 20
formula = z ~ normal(mean ~ 1 + type, sd  ~ 1)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          type ~ normal(0, 1)),
              sd   = list((Intercept) ~ gamma(1, 20)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999)) ->
  teacher_good_2


## =============================================================================
## With p-hacking (3)
## =============================================================================

formula = z ~ fixed(mean ~ 1 + type,
                    p ~ 1)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          type ~ normal(0, 1)),
              p    = list((Intercept) ~ beta(1, 1)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999)) ->
  teacher_fixed


## =============================================================================
## Without p-hacking (3)
## =============================================================================

data$dist_indices = 20
formula = z ~ fixed(mean ~ 1 + type)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          type ~ normal(0, 1)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999)) ->
  teacher_fixed_good


## =============================================================================
## With p-hacking (4)
## =============================================================================

formula = z ~ fixed(mean ~ 1,
                    p ~ 1)

priors = list(mean = list((Intercept) ~ normal(0, 1)),
              p    = list((Intercept) ~ beta(1, 1)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999)) ->
  teacher_fixed_2


## =============================================================================
## Without p-hacking (4)
## =============================================================================
data$dist_indices = 20
formula = z ~ fixed(mean ~ 1)

priors = list(mean = list((Intercept) ~ normal(0, 1)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.999)) ->
  teacher_fixed_good_2