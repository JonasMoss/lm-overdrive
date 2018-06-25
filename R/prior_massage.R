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

  checked_params   = match_formals(formals = formals,
                                     .args   = extract_arguments(call),
                                     .force  = .force)
  covariate = prior[[2]]

  covariates = list(deparse(covariate))

  if(!is.null(data) & deparse(covariate) != "(Intercept)") {
    if(is.factor(data[[covariate]])) {
      mod_matrix = stats::model.matrix(stats::as.formula(paste0("~", covariate)), data)
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
#' @return A list containing all the data needed for \code{straussR}-

massage_data = function(formula, priors, data = NULL) {
  ## Find out if p is included in the model or not according to the likelihoods.

  if(!is.null(data)) dist_indices = data$dist_indices
  include_p = if(!any(.database$includes_p[dist_indices])) FALSE else TRUE

  ## Collection of formulas on the right hand side in 'formula'.
  rhs = formula[[3]]
  formulas = lapply(X   = rhs[2:length(formula[[3]])],
                    FUN = stats::as.formula,
                    env = environment(formula))

  # First we identify the links and variable names of the formula.
  lhs_frame = do_call(dplyr::bind_rows, lapply(formulas, formula_lhs_list))
  links = as.list(stats::setNames(lhs_frame$link_integer, lhs_frame$variable))

  # Find the response variable name and get its value.
  Z = stats::model.frame(paste0("~", formula[[2]]), data = data)

  # Get the family.
  family_string = match.arg(deparse(formula[[3]][[1]]), names(.database$families))
  family = .database$families[[family_string]]$integer

  # Check for equality of priors and variable names:
  msg = paste0(c("The variables in the formula:",
                lhs_frame$variable, "and the variables in the priors:",
                names(priors), "do not match."), collapse = " ")

  assertthat::assert_that(all(sort(lhs_frame$variable) == sort(names(priors))),
                          msg = msg)

  # Check p is included among the variables when applicable:

  if(include_p) {
    msg = paste0("The variable 'p' is missing from the list of priors and the",
                 " formula. ")
    assertthat::assert_that("p" %in%  names(priors), msg = msg)
  } else {
    msg = paste0("The variable 'p' is included in the list of priors and the",
                 " formula, but there is no likelihood that depends on p.")
    assertthat::assert_that(!("p" %in%  names(priors)), msg = msg)
  }

  # Get distribution family and match it with the table.
  family_name = formula[[3]][[1]]
  family_object = .database$families[[deparse(family_name)]]
  msg = paste0("The familt object '", deparse(family_name), "' is not ",
               "recognized. See the documentation for supported families.")
  assertthat::assert_that(!is.null(family_object), msg = msg)

  # Now we check that the supplied formula matches the definition of the
  # chosen family.

  formals = alist()

  for(param in names(family_object$parameters)) {
    formals[[param]] = substitute()
  }

  if(include_p) formals$p = substitute()

  formula_arguments = rep(0, length(lhs_frame$variable))
  names(formula_arguments) = unlist(lhs_frame$variable)

  order = names(match_formals(formals = formals, .args = formula_arguments))

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

  link_names = names(.database$links)
  link_integers = sapply(.database$links, function(l) l$integer)
  link_strings = sapply(links, function(l) link_names[link_integers == l])

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

  sdata = c(model_matrix(formula, priors, data),
    list(Z                     = unlist(Z),
         family                = family,
         family_string         = family_string,
         link_types            = array(links),
         link_strings          = link_strings,
         unbounded_prior       = unbounded_prior,
         positive_prior        = positive_prior,
         unit_prior            = unit_prior,
         unbounded_prior_types = unbounded_prior_types,
         positive_prior_types  = positive_prior_types,
         unit_prior_types      = unit_prior_types))


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

  sdata$no_unbounded = array(rowSums(sdata$unbounded_indices))
  sdata$no_positive  = array(rowSums(sdata$positive_indices))
  sdata$no_unit      = array(rowSums(sdata$unit_indices))

  sdata

}

#' Convert a collection of formulas to a model matrix and a matrix of indices.
#'
#' @param formulas A list of formulas.
#' @return A list containing a model matrices and a matrices of indices.

model_matrix = function(formula, priors, data = NULL) {

  rhs = formula[[3]]
  formulas = lapply(X   = rhs[2:length(formula[[3]])],
                    FUN = stats::as.formula,
                    env = environment(formula))

  terms = lapply(formulas, formula_labels, include_intercept = TRUE)

  uniques = unique(unlist(terms))

  if("(Intercept)" %in% uniques) {
    uniques = setdiff(uniques, "(Intercept)")
  } else {
    uniques = c("0", uniques)
  }

  if(length(uniques) == 0) uniques = c(1)
  formula_str = paste("~", do.call(paste, as.list(c(sep = " + ", uniques))))

  model = stats::model.matrix(stats::as.formula(formula_str), data)
  index_matrix = matrix(0, nrow = length(terms), ncol = ncol(model))

  domains = lapply(priors, function(prior) {
    massage_priors(prior, data = data)$prior_domain
  })

  for(i in 1:length(domains)) {
    index_matrix[i, match(names(domains[[i]]), colnames(model))] = 1
  }

  colnames(index_matrix) = colnames(model)

  Q = ncol(index_matrix)
  P = nrow(index_matrix)


  unbounded_indices = array(data = 0, dim = dim(index_matrix))
  positive_indices  = array(data = 0, dim = dim(index_matrix))
  unit_indices      = array(data = 0, dim = dim(index_matrix))

  all_names = colnames(index_matrix)

  for(i in 1:length(domains)) {
    unbounded_names = names(domains[[i]][domains[[i]] == "unbounded"])
    unbounded_indices[i, match(unbounded_names, all_names)] =
      index_matrix[i, match(unbounded_names, all_names)]

    positive_names = names(domains[[i]][domains[[i]] == "positive"])
    positive_indices[i, match(positive_names, all_names)] =
      index_matrix[i, match(positive_names, all_names)]

    unit_names = names(domains[[i]][domains[[i]] == "unit"])
    unit_indices[i, match(unit_names, all_names)] =
      index_matrix[i, match(unit_names, all_names)]
  }

  colnames(unbounded_indices) = colnames(model)
  colnames(positive_indices)  = colnames(model)
  colnames(unit_indices)      = colnames(model)

  list(X                 = model,
       unbounded_indices = unbounded_indices,
       positive_indices  = positive_indices,
       unit_indices      = unit_indices)

}

#' Convenience functions for formulas.
#'
#' @param formula A formula object
#' @details The function \code{formula_lhs_list} identifies the integer code and
#'     name of the link function, and the variable of a formula and returns them
#'     in a named list.

formula_lhs_list = function(formula) {

  lhs = formula[[2]]

  if(length(lhs) == 1) {
    fun  = quote(identity)
    var  = deparse(lhs)
  } else if (length(lhs) == 2) {
    fun  = lhs[[1]]
    var  = deparse(lhs[[2]])
  } else if (length(lhs) == 3) {
    fun  = lhs[[1]]
    var  = deparse(lhs[[3]])
  }

  link_integer = -Inf

  for (elem in .database$links) {
    if(any(unlist(lapply(elem$keys, function(key) fun == key)))) {
      link_integer = elem$integer
      link_name    = deparse(elem$keys[[1]])
      break
    }

  }

  msg = paste0("Can't recognize the link '", deparse(fun), "'. Check the ",
               "documentation for available links.")

  assertthat::assert_that(link_integer != -Inf, msg = msg)


  list(link_integer = link_integer,
       link_name    = link_name,
       variable     = var)

}
