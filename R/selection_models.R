#' Fixed and mixed effects meta-analysis under selection for significance
#'
#' @param d Vector of nbserved effect sizes.
#' @param n Integer vector of sample sizes.
#' @param c Vector of lower cut-off values.
#' @param type String specifying the type of analysis.
#' @return A vector of estimates and an estimate of its asymptotic covariance
#'    matrix. For the fixed effects model, the estimate is the estimated mean.
#'    For the mixed effects model, the estimates are the mean and the variance
#'    of the normal effect size distribution.

pcurve = function(d, n, c = rep(stats::qnorm(0.975), length(n)), type = "mixed") {

  z = sqrt(n)*d

  if(type == "mixed") {

    objective= function(p) -mean(dtruncnorm(x = z,
                                            mean = sqrt(n)*p[1],
                                            sd = sqrt(1 + n*p[2]),
                                            a = c,
                                            b = Inf,
                                            log = TRUE))

    result = stats::nlm(objective, p = c(1, 1), hessian = TRUE)
    estimate = result$estimate
    attr(estimate, "variance") = solve(result$hessian)

  } else if (type == "fixed") {

    objective= function(p) -mean(dtruncnorm(x = z,
                                            mean = sqrt(n)*p,
                                            sd = 1,
                                            a = c,
                                            b = Inf,
                                            log = TRUE))

    result = stats::optimize(objective, lower = -10, upper = 10)
    estimate = result$minimum
    attr(estimate, "variance") = 1/numDeriv::hessian(objective, estimate)[1]
  }

  estimate
}
