
reps = 100
m = 100

replicate(reps, {
  N = 100
  theta = rnorm(N, 0.1, 0.2)
  n = sample(20:80, N, replace = TRUE)
  n = rep(m, N)
  z = truncnorm::rtruncnorm(N, mean = sqrt(n)*theta, sd = 1, a = qnorm(0.975))
  value = pcurve(z/sqrt(n), n)
  c(value[1],
    attr(value, "variance")[1, 1],
    value[2])
}) -> res

hist(res[1, ])
hist(sqrt(res[3, ]))



N = 10
m = 1
theta = rnorm(N, 0.1, 0.2)
n = sample(20:80, N, replace = TRUE)
n = rep(m, N)
z = truncnorm::rtruncnorm(N, mean = sqrt(n)*theta, sd = 1, a = qnorm(0.975))
c = 1.96
objective = function(x, y) {
  theta0 = x
  tau_sqrd = y
  mean(dtruncnorm(x = z,
              mean = sqrt(n)*theta0,
              sd = sqrt(1 + n*tau_sqrd),
              a = c, log = TRUE))
}

x = seq(-1, -0.5, length.out = 100)
y = seq(0.001, 2, length.out = 100)
z1 = outer(x, y, Vectorize(objective))
#contour(x, y, z1)


x = seq(-10, 30, length.out = 1000)
plot(x, Vectorize(objective)(x, y = 0.01), type = "l")
abline(v = 0)

y = seq(0.001, 2, length.out = 1000)
plot(y, Vectorize(objective)(x = 0.1, y), type = "l")
abline(v = 0.2^2)
library("plotly")

x = seq(-200, 0, length.out = 500)
y = seq(0.1, 200, length.out = 500)
z1 = outer(x, y, Vectorize(objective))
plot_ly(x = x, y = sqrt(y), z = z1, type = "surface")
pcurve(z/sqrt(n), n)

contour(x, log(y), z1)
persp(x, log(y), z1)

contour(x, log(y), z1, levels = c(-0.55, -0.6, -1, -2, -3, -10, -20, -30, -50))


dtnorm = function(x, mean, sd, a = -Inf, log = FALSE) {
  if(!log) truncnorm::dtruncnorm(x, mean = mean, sd = sd, a = a)
  else dnorm(x, mean, sd, log = TRUE) - pnorm((mean - a)/sd, log.p = TRUE)
}