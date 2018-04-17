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


#' Massages \code{formula}, \code{priors}, and \code{data} for \code{straussR}.
#'
#' Converts the information contained in \code{formula}, \code{priors},
#' and \code{data} for \code{straussR} to a form useable by \code{straussR}.
#' Checks for compatibility between priors and formulas and does some error
#' checking.
#'
#' @param formula A formula of the form given to \code{straussR}.
#' @param priors A list of priors. Should match the parameters of
#' \code{formula}.
#' @param data An optional data frame.
#'
#' @return Not sure.

massage_priors = function(formula, priors, data = NULL) {

  ## Collection of formulas on the right hand side in 'formula'.
  formulas = lapply(X   = rhs[2:length(formula[[3]])],
                    FUN = as.formula,
                    env = environment(formula))

  # First we identify the links and variable names of the formula.
  lhs_frame = do_call(dplyr::bind_rows, lapply(formulas, formula_lhs_list))

  # Check for equality of priors and variable names:
  msg = paste0(c("The variables in the formula:",
                lhs_frame$variable, "and the variables in the priors:",
                names(priors), "do not match."), collapse = " ")

  assertthat::assert_that(all(sort(lhs_frame$variable) == sort(names(priors))),
                          msg = msg)

  # Check p is included among the variables:

  msg = paste0("The variable 'p' is missing from the list of priors and the
               formula. ")
  assertthat::assert_that("p" %in%  names(priors), msg = msg)

  # Get distribution family and match it with the table.
  family_name = formula[[3]][[1]]
  family_object = family_list[[deparse(family_name)]]
  msg = paste0("The familt object '", deparse(family_name), "' is not ",
               "recognized. See the documentation for supported families.")
  assertthat::assert_that(!is.null(family_object), msg = msg)

  # Now we check that the supplied formula matches the definition of the
  # chosen family.

  formals = alist(mean =, sd =)

  for(param in names(family_object$extra_parameters)) {
    formals[[param]] = substitute()
  }

  formals$p = substitute()

  formula_arguments = rep(0, length(lhs_frame$variable))
  names(formula_arguments) = unlist(lhs_frame$variable)

  order = names(check_signature(formals = formals, .args = formula_arguments))

  # We use the 'order' variable to reorder the formulas and priors before
  # further processing.

  new_priors    = NULL
  new_formulas  = NULL

  for(name in order) {
    new_priors[[name]] = priors[[name]]
    new_formulas[name] = formulas[lhs_frame$variable == name]
  }

  priors   = new_priors
  formulas = new_formulas
  rm(new_priors)
  rm(new_formulas)

  priors_massage  = lapply(priors, prior_massage)

  max_length_unbounded = max(sapply(priors_massage, function(elem) {
    length(elem$prior_domain[elem$prior_domain == "unbounded"])
  }))

  max_length_positive = max(sapply(priors_massage, function(elem) {
    length(elem$prior_domain[elem$prior_domain == "positive"])
  }))

  max_length_unit = max(sapply(priors_massage, function(elem) {
    length(elem$prior_domain[elem$prior_domain == "unit"])
  }))

  unbounded_prior = array(data = 0, dim = c(max_length_unbounded, MP, length(priors)))
  positive_prior  = array(data = 0, dim = c(max_length_positive, MP, length(priors)))
  unit_prior      = array(data = 0, dim = c(max_length_unit, MP, length(priors)))

  unbounded_prior_types = array(data = 0, dim = c(max_length_unbounded, length(priors)))
  positive_prior_types  = array(data = 0, dim = c(max_length_positive, length(priors)))
  unit_prior_types      = array(data = 0, dim = c(max_length_unit, length(priors)))

  unbounded_prior_types = array(data = 0, dim = c(length(priors), max_length_unbounded))
  positive_prior_types  = array(data = 0, dim = c(length(priors), max_length_positive))
  unit_prior_types      = array(data = 0, dim = c(length(priors), max_length_unit))


  index = 1
  for(prior in priors_massage) {

    i_unbounded = 1
    i_positive  = 1
    i_unit      = 1

    for(i in seq_along(prior$prior_domain)) {

      if(prior$prior_domain[[i]] == "unbounded") {

        unbounded_prior[i_unbounded, 1:6, index] = prior$prior_parameters[i, ]
        unbounded_prior_types[index, i_unbounded] = prior$prior_types[i]
        i_unbounded = i_unbounded + 1

      } else if(prior$prior_domain[[i]] == "positive") {

        positive_prior[i_positive, 1:6, index] = prior$prior_parameters[i, ]
        positive_prior_types[index, i_positive] = prior$prior_types[i]
        i_positive = i_positive + 1

      } else if(prior$prior_domain[[i]] == "unit") {

        unit_prior[i_unit, 1:6, index] = prior$prior_parameters[i, ]
        unit_prior_types[index, i_unit] = prior$prior_types[i]
        i_unit = i_unit + 1

      }
    }

    index = index + 1
  }

  list(unbounded_prior       = unbounded_prior,
       positive_prior        = positive_prior,
       unit_prior            = unit_prior,
       unbounded_prior_types = unbounded_prior_types,
       positive_prior_types  = positive_prior_types,
       unit_prior_types      = unit_prior_types)

}