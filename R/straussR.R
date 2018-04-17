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
  model = family_formula_to_list2(formula)

  model_list = model_matrix(model$rhs_list,
                            priors = priors,
                            data = data)

  priors_massage = prior_list_massage(priors)

  sdata = list()

  sdata$M            = data$M
  sdata$lower_bounds = data$lower
  sdata$upper_bounds = data$upper
  sdata$dist_indices = data$dist_indices

  sdata$MAX_PAR = MP
  sdata$N       = nrow(model_list$X)
  sdata$P       = ncol(model_list$unbounded_indices)
  sdata$Q       = nrow(model_list$unbounded_indices)
  sdata$family  = model$family

  indices = sapply(family_list, function(elem) elem$integer) == model$family
  family_domain = family_list[[names(which(indices))]]$domain
  sdata$family_type = switch(family_domain,
                             "unbounded" = 0,
                             "positive"  = 1,
                             "unit"      = 2)

  sdata$N_unbounded = if(family_domain  == "unbounded") sdata$N else 0
  sdata$N_positive  = if(family_domain  == "positive") sdata$N else 0
  sdata$N_unit      = if(family_domain  == "unit") sdata$N else 0

  sdata$Z       = model.frame(model$response, data = data)[, 1]
  sdata$X       = model_list$X

  sdata$no_unbounded = rowSums(model_list$unbounded_indices)
  sdata$no_positive  = rowSums(model_list$positive_indices)
  sdata$no_unit      = rowSums(model_list$unit_indices)

  sdata$link_types   = model$link_types

  sdata$unbounded_indices     = model_list$unbounded_indices
  sdata$unbounded_prior       = priors_massage$unbounded_prior
  sdata$unbounded_prior_types = priors_massage$unbounded_prior_types

  sdata$positive_indices     = model_list$positive_indices
  sdata$positive_prior       = priors_massage$positive_prior
  sdata$positive_prior_types = priors_massage$positive_prior_types

  sdata$unit_indices     = model_list$unit_indices
  sdata$unit_prior       = priors_massage$unit_prior
  sdata$unit_prior_types = priors_massage$unit_prior_types

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

