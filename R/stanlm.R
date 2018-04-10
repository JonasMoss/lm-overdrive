stanlm_model = rstan::stan_model(file       = "src/stan_files/stanlm.stan",
                                 model_name = "stanlm")

#' Linear regression with STAN.
#'
#' Linear regression with STAN with some options.
#'
#' @param formula The formula.
#' @param data The data.
#' @param p_formula Formula for p.
#' @param mean_priors List of formulas. Priors for the coefficients for
#' \code{mean}.
#' @param sd_priors List of formulas. Priors for the coefficients for
#' \code{sd}.
#' @param p_priors List of formulas. Priors for the coefficients for
#' \code{p}.
#' @return A \code{STAN} object.

stanlm = function(formula, data, p_formula, mean_priors = NULL,
                  sd_priors = NULL, p_priors = NULL) {

  if(missing(data)) data = NULL

  model = family_formula_to_list(formula)
  p_model = formula_to_list(p_formula, type = "p")

  mean_priors_massage = prior_massage(mean_priors)
  sd_priors_massage = prior_massage(sd_priors)
  prob_priors_massage = prior_massage(p_priors)
  extra_priors_massage = prior_massage(model$priors)

  sdata = list()

  sdata$y      = model.frame(model$response, data = data)[, 1]
  sdata$n      = length(sdata$y)
  sdata$mp     = 6
  sdata$family = family_list[[model$family]]$integer

  ## ===========================================================================
  ## Boilerplate warning: Repetition of code ahead.
  ## ===========================================================================

  ## Adding the mean data.

  mean_types = mean_priors_massage$prior_types

  mean_unbounded_indices = mean_types > 99  & mean_types < 200
  mean_positive_indices  = mean_types > 199 & mean_types < 500
  mean_unit_indices   = mean_types > 499 & mean_types < 600

  x_mean = model.matrix(model$mean_formula, data = data)

  sdata$mean_link_type   = model$mean_link_type
  sdata$x_mean_unbounded = x_mean[, mean_unbounded_indices, drop = FALSE]
  sdata$x_mean_positive  = x_mean[, mean_positive_indices, drop = FALSE]
  sdata$x_mean_unit      = x_mean[, mean_unit_indices, drop = FALSE]
  sdata$p_mean_unbounded = sum(mean_unbounded_indices)
  sdata$p_mean_positive  = sum(mean_positive_indices)
  sdata$p_mean_unit   = sum(mean_unit_indices)
  sdata$prior_mean_unbounded = mean_priors_massage$prior_parameters[mean_unbounded_indices, , drop = FALSE]
  sdata$prior_mean_positive  = mean_priors_massage$prior_parameters[mean_positive_indices, , drop = FALSE]
  sdata$prior_mean_unit   = mean_priors_massage$prior_parameters[mean_unit_indices, , drop = FALSE]
  sdata$prior_types_mean_unbounded = array(mean_types[mean_unbounded_indices])
  sdata$prior_types_mean_positive  = array(mean_types[mean_positive_indices])
  sdata$prior_types_mean_unit   = array(mean_types[mean_unit_indices])

  ## Adding the sd data.

  sd_types = sd_priors_massage$prior_types

  sd_unbounded_indices = sd_types > 99  & sd_types < 200
  sd_positive_indices  = sd_types > 199 & sd_types < 500
  sd_unit_indices   = sd_types > 499 & sd_types < 600

  x_sd = model.matrix(model$sd_formula, data = data)
  sdata$sd_link_type   = model$sd_link_type
  sdata$x_sd_unbounded = x_sd[, sd_unbounded_indices, drop = FALSE]
  sdata$x_sd_positive  = x_sd[, sd_positive_indices, drop = FALSE]
  sdata$x_sd_unit      = x_sd[, sd_unit_indices, drop = FALSE]
  sdata$p_sd_unbounded = sum(sd_unbounded_indices)
  sdata$p_sd_positive  = sum(sd_positive_indices)
  sdata$p_sd_unit   = sum(sd_unit_indices)
  sdata$prior_sd_unbounded = sd_priors_massage$prior_parameters[sd_unbounded_indices, , drop = FALSE]
  sdata$prior_sd_positive  = sd_priors_massage$prior_parameters[sd_positive_indices, , drop = FALSE]
  sdata$prior_sd_unit   = sd_priors_massage$prior_parameters[sd_unit_indices, , drop = FALSE]
  sdata$prior_types_sd_unbounded = array(sd_types[sd_unbounded_indices])
  sdata$prior_types_sd_positive  = array(sd_types[sd_positive_indices])
  sdata$prior_types_sd_unit   = array(sd_types[sd_unit_indices])

  ## Adding the probs data.

  prob_types = prob_priors_massage$prior_types

  prob_unbounded_indices = prob_types > 99  & prob_types < 200
  prob_positive_indices  = prob_types > 199 & prob_types < 500
  prob_unit_indices   = prob_types > 499 & prob_types < 600

  x_prob = model.matrix(p_model$p_formula, data = data)
  sdata$prob_link_type   = p_model$p_link_type
  sdata$x_prob_unbounded = x_prob[, prob_unbounded_indices, drop = FALSE]
  sdata$x_prob_positive  = x_prob[, prob_positive_indices, drop = FALSE]
  sdata$x_prob_unit      = x_prob[, prob_unit_indices, drop = FALSE]
  sdata$p_prob_unbounded = sum(prob_unbounded_indices)
  sdata$p_prob_positive  = sum(prob_positive_indices)
  sdata$p_prob_unit   = sum(prob_unit_indices)
  sdata$prior_prob_unbounded = prob_priors_massage$prior_parameters[prob_unbounded_indices, , drop = FALSE]
  sdata$prior_prob_positive  = prob_priors_massage$prior_parameters[prob_positive_indices, , drop = FALSE]
  sdata$prior_prob_unit   = prob_priors_massage$prior_parameters[prob_unit_indices, , drop = FALSE]
  sdata$prior_types_prob_unbounded = array(prob_types[prob_unbounded_indices])
  sdata$prior_types_prob_positive  = array(prob_types[prob_positive_indices])
  sdata$prior_types_prob_unit   = array(prob_types[prob_unit_indices])


  ## Adding the extra parameters data.

  extra_types = extra_priors_massage$prior_types

  extra_unbounded_indices = extra_types > 99  & extra_types < 200
  extra_positive_indices  = extra_types > 199 & extra_types < 500
  extra_unit_indices   = extra_types > 499 & extra_types < 600

  sdata$p_extra_unbounded = sum(extra_unbounded_indices)
  sdata$p_extra_positive  = sum(extra_positive_indices)
  sdata$p_extra_unit   = sum(extra_unit_indices)
  sdata$prior_extra_unbounded = extra_priors_massage$prior_parameters[extra_unbounded_indices, , drop = FALSE]
  sdata$prior_extra_positive  = extra_priors_massage$prior_parameters[extra_positive_indices, , drop = FALSE]
  sdata$prior_extra_unit   = extra_priors_massage$prior_parameters[extra_unit_indices, , drop = FALSE]
  sdata$prior_types_extra_unbounded = array(extra_types[extra_unbounded_indices])
  sdata$prior_types_extra_positive  = array(extra_types[extra_positive_indices])
  sdata$prior_types_extra_unit   = array(extra_types[extra_unit_indices])

  # Done with data insertion, time for sampling!
  stan_fit = do_call(rstan::sampling,
                     object = stanlm_model,
                     data   = sdata)
  stan_fit
}
