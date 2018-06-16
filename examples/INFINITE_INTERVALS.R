alpha = 0.05
mu = -10000
sigma = abs(mu)
a_mu = qnorm(1 - alpha)*sigma + mu
pnorm((a_mu - mu)/sigma)

xx = seq(-5, 5, by = 0.01)
plot(xx, dnorm(xx, -1, 1), type = "l")
lines(xx, dnorm(xx, -1, 2))
lines(xx, dnorm(xx, -1, 3))
lines(xx, dnorm(xx, -1, 10))
abline(v = qnorm(alpha/2), lty = 2)
abline(v = qnorm(1 - alpha/2), lty = 2)

a = 0.1
mu = -5
x = seq(1, 10, by = 0.01)
G = function(sigma) pnorm(-(mu + a)/sigma) + pnorm((mu  - a)/sigma)
g = function(sigma) -1/sigma^2*(-(mu + a)*dnorm(-(mu + a)/sigma) + (mu - a)*dnorm((mu - a)/sigma))
plot(x, G(x), type = "l")
abline(v = abs(mu))

res = sapply(x, function(x) numDeriv::grad(G, x))

mu = -100
sigma = abs(mu)
G = function(a) pnorm(-(mu + a)/sigma) + pnorm((mu - a)/sigma)
plot(x, G(x))
abline(h = 0.95)

H = function(a) -1/sigma*(dnorm(-(mu + a)/sigma) + dnorm((mu - a)/sigma))
plot(x, H(x))


## =============================================================================
## Solution to stuff.
## =============================================================================

alpha = 0.05
x = seq(0, 2, by = 0.01)
plot(x, pnorm(1 - x) + pnorm(-1 - x), type = "l")
uniroot(function(x) (pnorm(1 -x) + pnorm(-1-x) - (1 - alpha)),
        interval = c(0, 1))

G = function(mu, sigma) pnorm(-(mu + mu/10)/sigma) + pnorm((mu - mu/10)/sigma)

H = function(a, sigma) 2*pnorm(-a/sigma)