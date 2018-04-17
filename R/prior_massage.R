#' Transforms a prior formula to a list.
#'
#' @param formula Two-sided formula with a covariate on the left-hand side and
#' a prior from \code{prior_list} on the right-hand side.
#' @param data Optional data frame.
#' @param .force Logical; if \code{TRUE}, forces the arguments given to the
#' prior.
#' @return A list containing list containing the name of the covariate, the name
#' of the prior,and the values of the prior parameters. If the right hand side
#' refers to a factor, the expanded factor names are used instead. (Intercept)
#' is not included: It must be named individually.

massage_prior = function(prior, data = NULL, .force = TRUE) {

  call       = prior[[3]]
  prior_name = deparse(extract_function_name(call))
  formals    = .database$priors[[prior_name]]$parameters

  checked_params   = check_signature(formals = formals,
                                     .args   = extract_arguments(call),
                                     .force  = .force)
  covariate = prior[[2]]

  covariates = list(deparse(covariate))

  if(!is.null(data) & deparse(covariate) != "(Intercept)") {
    if(is.factor(data[[covariate]])) {
      mod_matrix = model.matrix(as.formula(paste0("~", covariate)), data)
      covariates = colnames(mod_matrix)[-1]
    }
  }

  lapply(covariates, function(covariate) {
    list(covariate  = parse(text = covariate)[[1]],
         prior      = prior_name,
         parameters = checked_params)
    })

}

#' Transforms a list of priors to matrix form.
#'
#' @param priors List of priors.
#' @param data An optional data frame.
#' @return A list containing a matrix of prior parameters and a vector of
#' prior types.

massage_priors = function(prior, data = NULL) {

  if(is.null(prior)) return(list(prior_parameters = numeric(0),
                                 prior_types = integer(0)))

  if(length(prior) == 0) return(list(prior_parameters = numeric(0),
                                      prior_types = integer(0)))

  prior_list = do_call(c, .args = lapply(prior, massage_prior, data = data))

  prior_parameters = matrix(0, ncol = MAX_PAR, nrow = length(prior_list))

  for(i in 1:length(prior_list)) {
    prior_parameters[i, 1:length(prior_list[[i]]$parameters)] =
      prior_list[[i]]$parameters
  }

  rownames(prior_parameters) = sapply(prior_list, function(prior) {
    prior$covariate
  })

  prior_types = sapply(prior_list, function(elem) {
    .database$priors[[elem$prior]]$integer
  })

  prior_domain = sapply(prior_list, function(elem) {
    .database$priors[[elem$prior]]$domain
  })

  prior_names = sapply(prior_list, function(elem) elem$prior)

  covariates = sapply(prior_list, function(prior) prior$covariate)

  names(prior_types)  = covariates
  names(prior_domain) = covariates
  names(prior_names) = covariates

  list(prior_parameters = prior_parameters,
       prior_types      = prior_types,
       prior_names      = prior_names,
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

massage_data = function(formula, priors, data = NULL) {

  ## Collection of formulas on the right hand side in 'formula'.
  formulas = lapply(X   = rhs[2:length(formula[[3]])],
                    FUN = as.formula,
                    env = environment(formula))

  # First we identify the links and variable names of the formula.
  lhs_frame = do_call(dplyr::bind_rows, lapply(formulas, formula_lhs_list))
  links = as.list(stats::setNames(lhs_frame$link_integer, lhs_frame$variable))

  # Find the response variable name and get its value.
  Z = model.frame(paste0("~", formula[[2]]), data = data)

  # Get the family.
  family_string = match.arg(deparse(formula[[3]][[1]]), names(.database$families))
  family = c(family_string = .database$families[[family_string]]$integer)

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
  # further processing. We must order the priors, formulas, and links.

  new_priors    = NULL
  new_formulas  = NULL
  new_links     = NULL

  for(name in order) {
    new_priors[[name]] = priors[[name]]
    new_formulas[name] = formulas[lhs_frame$variable == name]
    new_links[[name]]  = links[[name]]
  }

  priors   = new_priors
  formulas = new_formulas
  links    = new_links

  rm(new_priors)
  rm(new_formulas)
  rm(new_links)

  # Now we must handle the many lists of priors:
  massaged_priors = lapply(X = priors,
                           FUN = massage_priors,
                           data = data)

  max_length_unbounded = max(sapply(massaged_priors, function(elem) {
    length(elem$prior_domain[elem$prior_domain == "unbounded"])
  }))

  max_length_positive = max(sapply(massaged_priors, function(elem) {
    length(elem$prior_domain[elem$prior_domain == "positive"])
  }))

  max_length_unit = max(sapply(massaged_priors, function(elem) {
    length(elem$prior_domain[elem$prior_domain == "unit"])
  }))

  L = length(priors)

  unbounded_prior = array(data = 0, dim = c(max_length_unbounded, MAX_PAR, L))
  positive_prior  = array(data = 0, dim = c(max_length_positive, MAX_PAR, L))
  unit_prior      = array(data = 0, dim = c(max_length_unit, MAX_PAR, L))

  unbounded_prior_types = array(data = 0, dim = c(max_length_unbounded, L))
  positive_prior_types  = array(data = 0, dim = c(max_length_positive, L))
  unit_prior_types      = array(data = 0, dim = c(max_length_unit, L))

  unbounded_prior_types = array(data = 0, dim = c(L, max_length_unbounded))
  positive_prior_types  = array(data = 0, dim = c(L, max_length_positive))
  unit_prior_types      = array(data = 0, dim = c(L, max_length_unit))


  index = 1
  for(prior in massaged_priors) {

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

  list(Z                     = unlist(Z),
       family                = family,
       link_types            = links,
       unbounded_prior       = unbounded_prior,
       positive_prior        = positive_prior,
       unit_prior            = unit_prior,
       unbounded_prior_types = unbounded_prior_types,
       positive_prior_types  = positive_prior_types,
       unit_prior_types      = unit_prior_types)

}