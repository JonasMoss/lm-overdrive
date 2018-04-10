priors_change = function(priors) {
  prior_list = vector("list", length(priors))

  for(i in 1:length(priors)) {
    prior_list[[i]] = prior_formula_to_list(priors[[i]])
  }

  prior_list
}

#' Make a list from formulas.
#'
#' @param formula Two-sided formula with a covariate on the left-hand side and
#' a prior from \code{prior_list} on the right-hand side.
#' @param .force Logical; if \code{TRUE}, forces the arguments given to the
#' prior.
#' @return A list containing the name of the covariate, the name of the prior,
#' and the values of the prior parameters.

prior_formula_to_list = function(formula, .force = TRUE) {

  rhs        = formula[[3]]
  k          = length(formula[[3]]) - 1 # Number of parameters in the prior.
  prior_name = deparse(rhs[[1]])
  formals    = prior_list[[prior_name]]$parameters

  unchecked_params = sapply(1:k, function(i) rhs[[i + 1]])
  checked_params   = check_signature(formals = formals,
                                     .args   = unchecked_params,
                                     .force  = .force)

  list(covariate  = deparse(formula[[2]]),
       prior      = prior_name,
       parameters = checked_params)
}

#' Transforms a list of priors to matrix form.
#'
#' @param priors List of priors.
#' @return A list containing a matrix of prior parameters and a vector of
#' prior types.
prior_massage = function(priors) {
  priors_list = priors_change(priors)
  mp = max(sapply(priors_list, function(prior) length(prior$parameters)))
  prior_parameters = matrix(0, ncol = mp, nrow = length(priors_list))

  for(i in 1:length(priors_list)) {
    prior_parameters[i, 1:length(priors_list[[i]]$parameters)] =
      priors_list[[i]]$parameters
  }
  rownames(prior_parameters) = sapply(priors_list,
                                      function(prior) prior$covariate)
  prior_types = sapply(priors_list, function(prior) {
    prior_names_to_int(prior$prior)
  })

  prior_types_names = sapply(priors_list, function(prior) prior$prior)
  prior_types = data.frame(type      = prior_types,
                           name      = prior_types_names)
  prior_types_covariate = sapply(priors_list, function(prior) prior$covariate)
  row.names(prior_types) = prior_types_covariate

  list(prior_parameters = prior_parameters,
       prior_types       = prior_types)
}

prior_massage_to_data = function(priors, names) {
  prior_list = prior_massage(priors)
  parameters = prior_list$prior_parameters
  types = prior_list$prior_types
  list(prior_parameters = parameters[!is.na(match(rownames(parameters), names)), ],
       prior_types      = types[!is.na(match(rownames(types), names)), ])
}

priors =