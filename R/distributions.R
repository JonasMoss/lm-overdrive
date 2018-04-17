#' The Folded Normal Distribution
#'
#' Density, distribution function, quantile function, random generation, mean
#' function, variance function, and standard deviation function for the folded
#' normal distribution with underlying mean equal to \code{mean} and underlying
#' standard deviation equal to \code{sd}.
#'
#' @param x,q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If \code{length(n) > 1}, the length of the
#' vector is used.
#' @param mean Numeric vector of means.
#' @param sd Numeric vector of standard deviations.
#' @param log,log.p Logical; If \code{TRUE}, probabilities \code{p} are given as
#' \code{log(p)}.
#' @param lower.tail Logical; If \code{TRUE}, probabilities are
#' \eqn{P(X \leq x)}, otherwise \eqn{P(X \leq x)}.
#' @export
#' @name FoldedNormal
NULL

#' @rdname FoldedNormal
dfnorm = function(x, mean = 0, sd = 1, log = FALSE) {

  result = dnorm(x, mean, sd) + dnorm(x, -mean, sd)

  if(log) log(result) else result

}

#' @rdname FoldedNormal
pfnorm = function(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) {

  correction = if(lower.tail) 1 else 0

  result = pnorm(q, mean, sd, lower.tail, log.p = FALSE) +
    pnorm(q, -mean, sd, lower.tail, log.p = FALSE) -
    correction

  if(log.p) log(result) else result

}

#' @rdname FoldedNormal
rfnorm = function(n, mean = 0, sd = 1) {
  abs(rnorm(n, mean = mean, sd = sd))
}

#' @rdname FoldedNormal
efnorm = function(mean = 0, sd = 1) {
  sd*sqrt(2/pi)*exp(-mean^2/(2*sd^2)) + mean*(1 - 2*pnorm(-mean/sd))
}

#' @rdname FoldedNormal
vfnorm = function(mean = 0, sd = 1) {
  mean^2 + sd^2 - efnorm(mean = mean, sd = sd)^2
}

#' @rdname FoldedNormal
sdfnorm = function(mean = 0, sd = 1) sqrt(vfnorm(mean = mean, sd = sd))

#' The Truncated Folded Normal Distribution
#'
#' Density, distribution function, quantile function, and random generation for
#' the truncated folded normal distribution with underlying mean equal to
#' \code{mean}, underlying standard deviation equal to \code{sd}, lower bound
#' \code{a}, and upper bound \code{b}.
#'
#' @param x,q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If \code{length(n) > 1}, the length of the
#' vector is used.
#' @param mean Numeric vector of means.
#' @param sd Numeric vector of standard deviations.
#' @param a Numeric vector of lower bounds. May be \code{-Inf}.
#' @param b Numeric Vector of upper bounds. May be \code{Inf}.
#' @param log,log.p Logical; If \code{TRUE}, probabilities \code{p} are given as
#' \code{log(p)}.
#' @param lower.tail Logical; If \code{TRUE}, probabilities are
#' \eqn{P(X \leq x)}, otherwise \eqn{P(X \leq x)}.
#' @export
#' @name TruncatedFoldedNormal
NULL

#' @rdname TruncatedFoldedNormal
dtruncfnorm = function(x, mean = 0, sd = 1, a = 0, b = Inf, log = FALSE) {

  if(all(is.infinite(b)) & all(a == 0)) dfnorm(x, mean, sd, log)

  denominator = pfnorm(b, mean = mean, sd = sd) -
    pfnorm(a, mean = mean, sd = sd)

  if(log){
    log_numerator = dfnorm(x, mean, sd, log = TRUE)*(x >= a & x <= b) +
                    log((x >= a & x <= b))
    log_numerator - log(denominator)
  } else {
    numerator = dfnorm(x, mean, sd)*(x >= a & x <= b)
    numerator/denominator
  }

}

#' @rdname TruncatedFoldedNormal
rtruncfnorm = function(n, mean = 0, sd = 1, a = -Inf, b = Inf) {

  if(is.infinite(b) & is.infinite(-a)) rfnorm(n, mean, sd)
  assertthat::assert_that(a >= 0)
  assertthat::assert_that(b >= 0)

  # Split into positive and negative parts.
  #
  pos = pnorm((b - mean)/sd) - pnorm((a - mean)/sd)
  neg = pnorm((- a - mean)/sd) - pnorm((- b - mean)/sd)
  p = pos/(pos + neg)
  samples = rbinom(n, 1, p)

  truncnorm::rtruncnorm(n, mean = mean, sd = sd, a = a, b = b) * samples +
    abs(truncnorm::rtruncnorm(n, mean = mean, sd = sd, a = -b, b = -a)) *
    (1 - samples)
}


#' Double truncation of normal and folded normal distribution.
#'
#' @name DoubleTruncation
NULL

#' @rdname DoubleTruncation
dttruncnorm = function(x, mean = 0, sd = 1, a = 1, b = -1) {
  dnorm(x, mean, sd)*(x > a | x < b)
}

#' @rdname DoubleTruncation
rdtruncnorm = function(n, mean = 0, sd = 1, a = 1, b = -1) {
  numerator = pnorm((b - mean)/sd)
  denominator = pnorm((a - mean)/sd, lower.tail = FALSE) + numerator
  p = numerator/denominator
  samples = rbinom(n, 1, p)
  truncnorm::rtruncnorm(n, mean = mean, sd = sd, a = -Inf, b = b)*samples +
    truncnorm::rtruncnorm(n, mean = mean, sd = sd, a = a,    b = Inf)*(1-samples)
}

#' @rdname DoubleTruncation
rdtruncfnorm = function(n, mean = 0, sd = 1, a = 1, b = -1) {
  numerator = pfnorm(b, mean = mean, sd = sd)
  denominator = pfnorm(a, mean = mean, sd = sd, lower.tail = FALSE) + numerator
  p = numerator/denominator
  samples = rbinom(n, 1, p)
  rtruncfnorm(n, mean = mean, sd = sd, a = 0, b = b)*samples +
    rtruncfnorm(n, mean = mean, sd = sd, a = a,    b = Inf)*(1-samples)
}

#' Density of the truncated normal.
#'
#' A simple wrapper around truncnorm::dtruncnorm. In contrast to
#' truncnorm::dtruncnorm, this one supports the \code{log} argument.
dtruncnorm = function(x, mean, sd, a = -Inf, b = Inf, log = FALSE) {
  if(log) log(truncnorm::dtruncnorm(x, mean = mean, sd = sd, a = a, b = b))
  else truncnorm::dtruncnorm(x, mean = mean, sd = sd, a = a, b = b)
}