#' Convert a collection of formulas to a model matrix and a matrix of indices.
#'
#' @param formulas A list of formulas.
#' @return A list containing a model matrix and a matrix of indices.

model_matrix = function(formula, priors, data = NULL) {

  formulas = lapply(X   = rhs[2:length(formula[[3]])],
                    FUN = as.formula,
                    env = environment(formula))

  terms = lapply(formulas, formula_labels)

  uniques = unique(unlist(terms))

  if("(Intercept)" %in% uniques) {
    uniques = setdiff(uniques, "(Intercept)")
  } else {
    uniques = c("0", uniques)
  }

  if(length(uniques) == 0) uniques = c(1)
  formula_str = paste("~", do.call(paste, as.list(c(sep = " + ", uniques))))

  model = model.matrix(as.formula(formula_str), data)
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

#' Get term labels from a formula, intercept included.
#'

formula_labels = function(formula) {

  labels = attr(terms(formula), "term.labels")

  if(attr(terms(formula), "intercept") == 1) c("(Intercept)", labels)
  else labels

}

#' Convert a family formula
#'
#' Convert a family formula into a list containing the response, the
#' formula and link for the mean and standard deviation, and the prior for
#' the additional parameter(s). [Not implemented yet.]
#'
#' @param formula A formula object.
#' @return A list.

family_formula_to_list2 = function(formula) {
  rhs = formula[[3]]
  family = family_list[[deparse(rhs[[1]])]]$integer
  links = lapply(rhs[2:length(rhs)], formula_to_link)
  link_types      = sapply(links, function(elem) elem$link)
  link_parameters = sapply(links, function(elem) elem$var)

  env = environment(formula)

  rhs_list = lapply(as.list(rhs[2:length(rhs)]), as.formula, env = env)

  list(response        = as.formula(paste0("~", deparse(formula[[2]])), env = env),
       family          = family,
       link_types      = link_types,
       link_parameters = link_parameters,
       rhs_list        = rhs_list)
}

#' Convert a link-formula to a list.
#'
#' @param formula
#' @return List.

link_formula_to_list = function(formula) {
  rhs = formula[[3]]
  lhs = formula[[2]]
}


#' Convenience functions for formulas.
#'
#' @param formula A formula object
#' @detials The function \code{formula_lhs_list} identifies the integer code and
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

  for (elem in link_list2) {
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
