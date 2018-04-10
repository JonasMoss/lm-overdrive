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

  lhs = formula[[3]]

  family = match.arg(deparse(lhs[[1]]), names(family_list))

  arg_length = 2 + length(family_list[[family]]$extra_parameters)
  msg = paste0("The length of the argument vector (arity: ", length(lhs) - 1,
               ") passed to '", family, "' does not match its true arity (",
               "arity: ", arg_length, "). Look at 'family_list' or the STAN",
               " documentation for details.")
  assertthat::assert_that(arg_length == length(lhs) - 1, msg = msg)

  ## Now we handle the formula and link of the first argument.

  mean_formula = as.formula(lhs[[2]], env = env)
  sd_formula = as.formula(lhs[[3]], env = env)

  ## Now we handle potential additional arguments.
  # if(arg_length > 2) {
  #   for(i in (3:arg_length)) {
  #     current_formula = substituted[[i + 1]]
  #   }
  # }

  c(list(response = rlang::new_formula(rhs = 0, lhs = formula[[2]], env = env),
         family = family), formula_to_list(sd_formula, type = "sd"),
                           formula_to_list(mean_formula, type = "mean"))
}



do_fun = function(formula, data, p = NULL, mean_priors = NULL,
                  sd_priors = NULL, p_priors = NULL) {

  if(missing(data)) data = NULL
  model_list = family_to_stan(formula)
  if(!is.null(p)) p_list = link_formula_to_list(p, type = "p")

  y = unlist(model.frame(model_list$response, data))
  x_mean = model.matrix(model_list$mean_formula, data)
  x_sd   = model.matrix(model_list$sd_formula, data)
  x_p    = model.matrix(p_list$p_formula, data)
  list(y, x_mean, x_sd, x_p)
}


