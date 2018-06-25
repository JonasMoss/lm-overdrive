straussR_mixed = rstan::stan_model(file = "src/stan_files/straussR_mixed.stan",
                                   model_name = "straussR_mixed")

straussR_fixed = rstan::stan_model(file = "src/stan_files/straussR_fixed.stan",
                                   model_name = "straussR_fixed")

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
#' @param data An optional data frame. NOTE: Currently required.
#' @return A \code{STAN} object.
#' @export

straussR = function(formula, priors, data, ...) {

  dots = alist2(...)

  if(missing(data) | is.null(data)) {
    stop("Must supply some data")
    data = NULL
  }

  # This takes cae of all the data-massaging for mixed effects models.

  sdata = massage_data(formula, priors, data)

  if(is.null(dots$init)) {
    init = function(chain_id = 1) {
      list(thetas_unbounded = rep(0.1, sdata$N_unbounded),
           thetas_positive  = rep(0.1, sdata$N_positive),
           thetas_unit      = rep(0.1, sdata$N_unit))
    }
    dots$init = init
  }

  object = if(sdata$family == 0) straussR_fixed else straussR_mixed

  stan_object = do_call(.fn    = rstan::sampling,
                        .args  = dots,
                        object = object,
                        data   = sdata)

  return_object = list(stan_object = stan_object,
                       sdata       = sdata,
                       formula     = formula,
                       priors      = priors,
                       data        = data,
                       dots        = dots,
                       call        = match.call())

  class(return_object) = "straussR"
  return_object
}
