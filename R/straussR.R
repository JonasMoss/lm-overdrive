stan_straussR = rstan::stan_model(file       = "src/stan_files/straussR.stan",
                                 model_name = "straussR")


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
  sdata = c(massage_data(formula, priors, data),
            model_matrix(formula, priors, data))

  sdata$M            = data$M
  sdata$lower_bounds = data$lower
  sdata$upper_bounds = data$upper
  sdata$dist_indices = data$dist_indices

  sdata$MAX_PAR = MAX_PAR
  sdata$N       = nrow(sdata$X)
  sdata$P       = ncol(sdata$unbounded_indices)
  sdata$Q       = nrow(sdata$unbounded_indices)

  indices = sapply(.database$families, function(x) x$integer) == sdata$family
  family_domain = .database$families[[names(which(indices))]]$domain

  sdata$family_type = switch(family_domain,
                             "unbounded" = 0,
                             "positive"  = 1,
                             "unit"      = 2)

  sdata$N_unbounded = if(family_domain  == "unbounded") sdata$N else 0
  sdata$N_positive  = if(family_domain  == "positive") sdata$N else 0
  sdata$N_unit      = if(family_domain  == "unit") sdata$N else 0

  sdata$no_unbounded = rowSums(sdata$unbounded_indices)
  sdata$no_positive  = rowSums(sdata$positive_indices)
  sdata$no_unit      = rowSums(sdata$unit_indices)

  if(is.null(dots$init)) {
    no_unbounded = sum(sdata$no_unbounded)
    no_positive = sum(sdata$no_positive)
    no_unit = sum(sdata$no_unit)
    init = function(chain_id = 1) {
      list(thetas_unbounded = rep(0.1, sdata$N_unbounded),
           thetas_positive  = rep(0.1, sdata$N_positive),
           thetas_unit      = rep(0.1, sdata$N_unit))
    }
    dots$init = init
  }

  # Done with data insertion, time for sampling!
  stan_fit = do_call(rstan::sampling, .args = dots,
                     object = stan_straussR,
                     data   = sdata)
  stan_fit
}

