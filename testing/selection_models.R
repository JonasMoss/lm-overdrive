## z ~ fixed(mean ~ 1, p ~ 1)


N = 100
theta = 0.1
n = sample(20:80, N, replace = TRUE)
z = truncnorm::rtruncnorm(N, mean = sqrt(n)*theta, sd = 1, a = qnorm(0.975))

data              = data.frame(z = z)
data$M            = n
data$lower        = rep(qnorm(0.975), N)
data$upper        = rep(qnorm(0.975), N)
data$dist_indices = rep(25, N)

formula = z ~ fixed(mean ~ 1)
priors = list(mean = list((Intercept) ~ normal(0, 1)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.99)) -> mods


hist(rstan::extract(mods)$beta_unbounded)

N = 1000
theta = truncnorm::rnorm(N, mean = 0.1, sd = 0.1, a = 0, b = Inf)
n = sample(20:80, N, replace = TRUE)
z = truncnorm::rtruncnorm(N, mean = sqrt(n)*theta, sd = 1, a = qnorm(0.975))
mean(z/sqrt(n))
pcurve(z/sqrt(n), n, type = "mixed")
pcurve(z/sqrt(n), n, type = "fixed")


data              = data.frame(z = z)
data$M            = n
data$lower        = rep(qnorm(0.975), N)
data$upper        = rep(qnorm(0.975), N)
data$dist_indices = rep(25, N)

formula = z ~ truncnormal(mu ~ 1,
                          sigma ~ 1)

priors = list(mu = list((Intercept) ~ normal(0.1, 1)),
              sigma = list((Intercept) ~ gamma(1, 1)))


straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.99)) -> mods
plot(x, truncnorm::dtruncnorm(x, mean = -4.5, sd = 1, a = 0))
lines(x, truncnorm::dtruncnorm(x, mean = 0.1, sd = 0.1, a = 0))


formula = z ~ normal(mean ~ 1,
                     sd ~ 1,
                     p ~ 1)

priors = list(mean = list((Intercept) ~ gamma(1, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p   = list((Intercept) ~ beta(1, 1)))

straussR(formula = formula, data = data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.99)) -> mods

order = order(theta)
medians = apply(rstan::extract(mods)$thetas_unbounded, 2, median)
q05 = apply(rstan::extract(mods)$thetas_unbounded, 2, function(x) quantile(x, 0.05))
q95 = apply(rstan::extract(mods)$thetas_unbounded, 2, function(x) quantile(x, 0.95))

plot(theta[order], medians[order], ylim = c(-1, 1), pch = 20)
lines(theta[order], q05[order], col = "blue")
lines(theta[order], q95[order], col = "blue")
points(theta[order], z/sqrt(n), pch = 20, col = "red")
abline(a = 0, b = 1)
abline(h = mean(rstan::extract(mods)$thetas_unbounded))
abline(h = sum(z/sqrt(n)*(n/sum(n))), col = "red")



hist(rstan::extract(mods)$beta_positive[, 1])
hist(rstan::extract(mods)$beta_positive[, 2])

#
# reps = 100
# m = 100
#
# replicate(reps, {
#   N = 100
#   theta = rnorm(N, 0.1, 0.2)
#   n = sample(20:80, N, replace = TRUE)
#   n = rep(m, N)
#   z = truncnorm::rtruncnorm(N, mean = sqrt(n)*theta, sd = 1, a = qnorm(0.975))
#   value = pcurve(z, n)
#   c(value[1],
#     attr(value, "variance")[1, 1],
#     value[2])
# }) -> res
#
# hist(res[1, ])
# hist(sqrt(res[3, ]))
#
#
# objective = function(x, y) {
#   theta0 = x
#   tau_sqrd = y
#   mean(dtnorm(x = z,
#               mean = sqrt(n)*theta0,
#               sd = sqrt(1 + n*tau_sqrd),
#               a = c, log = TRUE))
# }
#
# x = seq(-1, -0.5, length.out = 100)
# y = seq(0.001, 2, length.out = 100)
# z1 = outer(x, y, Vectorize(objective))
# #contour(x, y, z1)
#
#
# x = seq(-10, 30, length.out = 1000)
# plot(x, Vectorize(objective)(x, y = 0.01), type = "l")
# abline(v = 0)
#
# y = seq(0.001, 2, length.out = 1000)
# plot(y, Vectorize(objective)(x = 0.1, y), type = "l")
# abline(v = 0.2^2)
# library("plotly")
#
# x = seq(-20, 0, length.out = 500)
# y = seq(0.1, 10, length.out = 500)
# z1 = outer(x, y, Vectorize(objective))
# plot_ly(x = x, y = sqrt(y), z = z1, type = "surface")
#
# contour(x, log(y), z1)
# persp(x, log(y), z1)
#
# contour(x, log(y), z1, levels = c(-0.55, -0.6, -1, -2, -3, -10, -20, -30, -50))
#
#
# dtnorm = function(x, mean, sd, a = -Inf, log = FALSE) {
#   if(!log) truncnorm::dtruncnorm(x, mean = mean, sd = sd, a = a)
#   else dnorm(x, mean, sd, log = TRUE) - pnorm((mean - a)/sd, log.p = TRUE)
# }
