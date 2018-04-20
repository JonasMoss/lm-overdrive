stan_straussR = rstan::stan_model(file       = "src/stan_files/straussR.stan",
                                 model_name  = "straussR")


#' Mixture-based Bayesian Meta-analysis
#'
#' \code{straussR} is used to obtain posteriors for mixture-based Bayesian
#' meta-analysis. Fixed effects, random effects, and meta-regressions are
#' supported.
#'
#' @param formula Model formula. Should have the \code{z} values on the left
#' hand side and the random effects distribution on the right hand side.
#' @param priors List of formulas. Should contain priors for all parameters
#' mentioned in \code{formula}.
#' @param data An optional data frame.
#' @return A \code{STAN} object.

straussR = function(formula, priors = NULL, data = NULL, ...) {

  dots = alist2(...)

  if(missing(data)) data = NULL

  sdata = massage_data(formula, priors, data)

  if(is.null(dots$init)) {
    init = function(chain_id = 1) {
      list(thetas_unbounded = rep(0.1, sdata$N_unbounded),
           thetas_positive  = rep(0.1, sdata$N_positive),
           thetas_unit      = rep(0.1, sdata$N_unit))
    }
    dots$init = init
  }

  do_call(rstan::sampling,
          .args  = dots,
          object = stan_straussR,
          data   = sdata)

}


