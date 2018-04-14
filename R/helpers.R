#' Matches the left-hand side of a formula object with a link.
#'
#' @param formula A formula object.
#' @param type The string used as a function argument.
#' @return A string, identifying a link in the 'link_list' list.

formula_to_link = function(formula, type = "mean") {

  formula_str = paste(as.character(formula)[2], "~",
                      as.character(formula)[3])
  msg = paste0("Expected formula, got object of class '", class(formula),
               "': ", formula_str, ".")
  assertthat::assert_that(rlang::is_formula(formula), msg = msg)
  is_two_sided(formula)

  formula_text = gsub(type, "x", deparse(attr(terms(formula), "variables")[[2]]))
  index = which(sapply(link_list, function(x) formula_text  %in% x$keys))
  link_name = names(link_list)[index]
  msg = paste0("The link supplied for '", type, "' is not supported: ",
               deparse(attr(terms(formula), "variables")[[2]]))
  assertthat::assert_that(length(link_name) != 0, msg = msg)
  link_name
}

#' Link-formula to link name and formula
#'
#' @param formula
#' @return List of link name and formula

formula_to_list = function(formula, type = "mean") {
  formula_str = paste(as.character(formula)[2], "~",
                      as.character(formula)[3])
  msg = paste0("Expected formula, got object of class '", class(formula),
               "': ", formula_str, ".")
  assertthat::assert_that(rlang::is_formula(formula), msg = msg)
  is_two_sided(formula)

  env = environment(formula)

  is_two_sided(formula)

  return_list = list()
  return_list[[paste0(type, "_link")]] =
    formula_to_link(formula, type = type)

  return_list[[paste0(type, "_link_type")]] =
    link_list[[formula_to_link(formula, type = type)]]$integer

  return_list[[paste0(type, "_formula")]] =
    rlang::new_formula(lhs = NULL, rhs = formula[[3]], env = env)

  return_list
}

#' Convert a family formula
#'
#' Convert a family formula into a list containing the response, the
#' formula and link for the mean and standard deviation, and the prior for
#' the additional parameter(s). [Not implemented yet.]
#'
#' @param formula A formula object.
#' @return A list.

family_formula_to_list = function(formula) {

  formula_str = paste(as.character(formula)[2], "~",
                      as.character(formula)[3])
  msg = paste0("Expected formula, got object of class '", class(formula),
               "': ", formula_str, ".")
  assertthat::assert_that(rlang::is_formula(formula), msg = msg)
  is_two_sided(formula)

  env = environment(formula)

  rhs = formula[[3]]

  family = match.arg(deparse(rhs[[1]]), names(family_list))

  arg_length = 2 + length(family_list[[family]]$extra_parameters)
  msg = paste0("The length of the argument vector (arity: ", length(rhs) - 1,
               ") passed to '", family, "' does not match its true arity (",
               "arity: ", arg_length, "). Look at 'family_list' or the STAN",
               " documentation for details.")
  assertthat::assert_that(arg_length == length(rhs) - 1, msg = msg)

  ## Now we handle the formula and link of the first argument.

  mean_formula = as.formula(rhs[[2]], env = env)
  sd_formula = as.formula(rhs[[3]], env = env)

  # Now we handle potential additional arguments.
  add_list = lapply(seq_len(arg_length - 2), function(i) {
    rhs[[3 + i]]
  })

  c(list(response = rlang::new_formula(rhs = 0, lhs = formula[[2]], env = env),
         family = family), formula_to_list(sd_formula, type = "sd"),
    formula_to_list(mean_formula, type = "mean"),
    list(priors = add_list))
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

  if(is.null(priors)) return(list(prior_parameters = numeric(0),
                                  prior_types = integer(0)))

  if(length(priors) == 0) return(list(prior_parameters = numeric(0),
                                      prior_types = integer(0)))

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

  prior_domain = sapply(priors_list, function(elem) {
    prior_list[[elem$prior]]$domain
  })

  covariates = sapply(priors_list, function(prior) prior$covariate)
  names(prior_types) = covariates
  names(prior_domain) = covariates

  list(prior_parameters = prior_parameters,
       prior_types      = prior_types,
       prior_domain     = prior_domain)
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
