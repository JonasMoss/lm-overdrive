#' Make a list from formulas.
#'
#' @param formula Two-sided formula with a covariate on the left-hand side and
#' a prior from \code{prior_list} on the right-hand side.
#' @param .force Logical; if \code{TRUE}, forces the arguments given to the
#' prior.
#' @return A list containing the name of the covariate, the name of the prior,
#' and the values of the prior parameters.

prior_formula_to_list = function(formula, .force = TRUE) {

  call       = formula[[3]]
  prior_name = deparse(extract_function_name(call))
  formals    = prior_list[[prior_name]]$parameters

  checked_params   = check_signature(formals = formals,
                                     .args   = extract_arguments(call),
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

  priors_list = lapply(priors, prior_formula_to_list)

  #mp = max(sapply(priors_list, function(prior) length(prior$parameters)))
  mp = 6

  prior_parameters = matrix(0, ncol = mp, nrow = length(priors_list))

  for(i in 1:length(priors_list)) {
    prior_parameters[i, 1:length(priors_list[[i]]$parameters)] =
      priors_list[[i]]$parameters
  }

  rownames(prior_parameters) = sapply(priors_list, function(prior) {
    prior$covariate
  })

  prior_types = sapply(priors_list, function(elem) {
    prior_list[[elem$prior]]$integer
  })

  covariates = sapply(priors_list, function(prior) prior$covariate)
  names(prior_types) = covariates

  list(prior_parameters = prior_parameters,
       prior_types      = prior_types)
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

  call       = formula[[3]]
  prior_name = deparse(extract_function_name(call))
  formals    = prior_list[[prior_name]]$parameters

  checked_params   = check_signature(formals = formals,
                                     .args   = extract_arguments(call),
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

  priors_list = lapply(priors, prior_formula_to_list)

  #mp = max(sapply(priors_list, function(prior) length(prior$parameters)))
  mp = 6

  prior_parameters = matrix(0, ncol = mp, nrow = length(priors_list))

  for(i in 1:length(priors_list)) {
    prior_parameters[i, 1:length(priors_list[[i]]$parameters)] =
      priors_list[[i]]$parameters
  }

  rownames(prior_parameters) = sapply(priors_list, function(prior) {
    prior$covariate
  })

  prior_types = sapply(priors_list, function(elem) {
    prior_list[[elem$prior]]$integer
  })

  covariates = sapply(priors_list, function(prior) prior$covariate)
  names(prior_types) = covariates

  list(prior_parameters = prior_parameters,
       prior_types      = prior_types)
}



#' Check the signature of a function.
#'
#' @param formals A list of formal arguments to match.
#' @param ... Arguments to match against \code{formals}.
#' @param .args Optional list of arguments to match against \code{formals}.
#' @param .force Logical; if \code{TRUE}, forces evaluation of all aruments in
#' \code{...} and \code{.args}.
#' @param .allow_defaults Logical; if \code{TRUE}, does not check for missing
#' values when defaults are present in \code{formals}. Not implemented yet.
#' @return A named vector containing correctly ordered arguments from \code{...}
#' and \code{.args}. Throws an error if this is not possible to do.

check_signature = function(formals,
                           ...,
                           .args = NULL,
                           .force = FALSE,
                           .allow_defaults = FALSE) {

  fun = function() {

    arguments = names(formals())
    k = length(arguments)
    env = new.env()

    for(i in 1:k) {
      env$x = parse(text = arguments[[i]])[[1]]
      msg   = paste0("The argument '", arguments[[i]], "' is missing.")
      assertthat::assert_that(!eval(substitute(missing(x), env = env)),
                              msg = msg)
    }

    call = match.call()

    if(.force) {
      parameters = sapply(2:(k+1), function(i) eval(call[[i]]))
    } else {
      parameters = sapply(2:(k+1), function(i) call[[i]])
    }

    names(parameters) = arguments
    parameters

  }

  formals(fun) = formals

  do_call(fun, .args = .args, ...)

}
