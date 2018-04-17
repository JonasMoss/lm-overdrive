#' Posterior predictive distribution of a \code{snifmar} object.
#'
#' @param x Numeric. A vector of points where the posterior predictive
#' distribution is to be evaluated.
#' @param object A \code{snifmar} object.
#' @return A vector of densities with the same arity as \code{x}.

dppredictive = function(x, object) {

  effect_distribution = object$effect_distribution

  if(effect_distribution == "normal") {

    theta  = rstan::extract(object$fit)$theta
    sigma  = rstan::extract(object$fit)$sigma

    result = sapply(x, function(x) mean(dnorm(x, theta, sigma)))

  } else if (effect_distribution == "skew_normal") {

    theta  = rstan::extract(object$fit)$theta
    sigma  = rstan::extract(object$fit)$sigma
    beta   = rstan::extract(object$fit)$beta

    result = sn::dsn(x, theta, sigma, beta)

  } else if (effect_distribution == "gumbel") {

    theta  = rstan::extract(object$fit)$theta
    beta   = rstan::extract(object$fit)$beta

    result = extraDistr::dgumbel(x, theta, beta)

  } else if (effect_distribution == "gamma") {

    mean_ = rstan::extract(object$fit)$mean_
    sd_   = rstan::extract(object$fit)$sd_
    var_  = sd_^2
    shape = mean_^2/var_
    rate  = mean_/var_

    result = dgamma(x, shape, rate)

  } else if (effect_distribution == "inverse_gaussian") {

    mean_  = rstan::extract(object$fit)$mean_
    sd_    = rstan::extract(object$fit)$sd_
    var_   = sd_^2
    mu     = mean_
    lambda = mean_^3/var_

    result = extraDistr::dwald(x, mu, lambda)
  }

  mean(result)

}

dppredictive = Vectorize(dppredictive, vectorize.args = c("x"))

#' Find the power distribution for a \code{snifmar} object.
#'
#' @param object A \code{snifmar} object.
#'
power_distribution = function(object, n, alpha = 1.96,
                              indices = "all", sample_n = FALSE) {

  thetas = rstan::extract(object)$thetas_positive
  N = ncol(thetas)
  n_sampled = if(sample_n) sample(n, N, replace = TRUE) else n

  if(all(indices == "all")) {
    c(sapply(1:N, function(i) {
      pnorm(1.96 - sqrt(n_sampled[i])*thetas[, i], lower.tail = FALSE)
    }))
  } else if (is.numeric(indices)) {
    c(sapply(indices, function(i) {
      pnorm(1.96 - sqrt(n_sampled[i])*thetas[, i], lower.tail = FALSE)
    }))
  }
}

#' Posterior effect size for a \code{snifmar} object.
#'
#' @param object A \code{snifmar} object.
#'
effect_distribution = function(object, alpha = 1.96,
                               indices = "all", sample_n = FALSE) {

  thetas_b = rstan::extract(object$fit)$thetas_b
  thetas_g = rstan::extract(object$fit)$thetas_g
  thetas = cbind(thetas_b, thetas_g)
  thetas[, object$stan_data$indices_b] = thetas_b
  thetas[, object$stan_data$indices_g] = thetas_g

  N = ncol(thetas)
  n = rep(NA, N)
  n[object$stan_data$indices_b] = object$stan_data$n_b
  n[object$stan_data$indices_g] = object$stan_data$n_g

  n_sampled = if(sample_n) sample(n, N, replace = TRUE) else n

  if(all(indices == "all")) {
    c(thetas)
  } else if (is.numeric(indices)) {
    c(sapply(indices, function(i) {
      c(thetas[, indices])
    }))
  }
}

#' Generic plot for \code{strauss} objects.
#'
#' @param obj A \code{strauss} object.
#' @param kind String. The kind of plot to make, see \code{details} for more.
#' @param options List. Options used by \code{kind}.
#' @param ... Passed to \code{plot.default}.

plot.strauss = function(obj, kind = "small_effects", options, ...) {
  if(kind == "small_effects") {
    plot(log(fit_test$stan_data$n_abs_b),
         log(fit_test1$stan_data$d_abs_b))
  }
}


small_effects_plot = function(obj, ...) {
  dots = alist2(...)

  args = add_elements(dots, log  = "xy",
                            xlab = "Sample size",
                            ylab = "Estimated effect size",
                            bty  = "l",
                            pch  = 20)

  x = c(obj$stan_data$n_abs_b,
        obj$stan_data$n_abs_g,
        obj$stan_data$n_l_b,
        obj$stan_data$n_r_b,
        obj$stan_data$n_g)

  y = c(colMeans(cbind(extract(obj$fit)$thetas_abs_b,
                       extract(obj$fit)$thetas_abs_g,
                       extract(obj$fit)$thetas_l_b,
                       extract(obj$fit)$thetas_r_b,
                       extract(obj$fit)$thetas_g)))

  do_call(plot, .args = args, x = x, y = y)

}