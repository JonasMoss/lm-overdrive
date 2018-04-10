#' Linear regression with STAN.
#'
#' Linear regression with STAN with some options.
#'
#' @param formula The formula.
#' @param data The data.
#' @param p Formula for p.
#' @param priors List of formulas. Priors for the coefficients.
#' @param link String. Link function.

stanlm = function(formula, data, priors) {

  if(missing(data)) data = NULL

  x = model.matrix(formula, data = data)
  y = model.response(model.frame(formula, data = data))

  p = ncol(x)
  n = nrow(x)

  prior_list = prior_massage_to_data(priors, names = colnames(x))
  types = prior_list$prior_types[, 1]

  unbounded_indices = types > 99  & types < 200
  positive_indices  = types > 199 & types < 500
  bounded_indices   = types > 499 & types < 600

  x_unbounded = x[, unbounded_indices, drop = FALSE]
  x_positive  = x[, positive_indices, drop = FALSE]
  x_bounded   = x[, bounded_indices, drop = FALSE]

  stan_data = list()
  stan_data$link_type   = link_type
  stan_data$x_unbounded = x_unbounded
  stan_data$x_positive  = x_positive
  stan_data$x_bounded   = x_bounded
  stan_data$family      = family_list[[family]]$integer

  stan_data$extra_positive_parameters = family_to_extra_positive_parameters(family)
  stan_data$extra_unbounded_parameters = family_to_extra_unbounded_parameters(family)
  stan_data$extra_unit_parameters = family_to_extra_unit_parameters(family)

  stan_data$y  = y
  stan_data$n  = n
  stan_data$mp = ncol(prior_list$prior_parameters)
  stan_data$p_unbounded = sum(unbounded_indices)
  stan_data$p_positive  = sum(positive_indices)
  stan_data$p_bounded   = sum(bounded_indices)
  stan_data$prior_unbounded = prior_list$prior_parameters[unbounded_indices, , drop = FALSE]
  stan_data$prior_positive  = prior_list$prior_parameters[positive_indices, , drop = FALSE]
  stan_data$prior_bounded   = prior_list$prior_parameters[bounded_indices, , drop = FALSE]
  stan_data$prior_types_unbounded = array(types[unbounded_indices])
  stan_data$prior_types_positive  = array(types[positive_indices])
  stan_data$prior_types_bounded   = array(types[bounded_indices])

  stan_fit = do_call(rstan::sampling,
                     object = stanlm_model,
                     data   = stan_data)
}

