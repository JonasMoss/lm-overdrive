### ============================================================================
### Handlers: This file contains functions that handle the stan objects. The
###    pure STAN objects are difficult to interpret since none of the parameters
###    have names. In addition, the pure parameters are not linked, so they
###    can't be used for prediction.
### ============================================================================

#' Extract parameter estimates from a \code{straussR} object.
#'
#' @param object A \code{straussR} object.
#' @param summary A function for summarizing the posterior samples of the
#' object. Defaults to \code{mean}.
#' @param show_effects Logical; If \code{TRUE}, shows the random effects.
#' @param unlist Logical; If \code{TRUE}, unlists each parameter summary.
#' @return A list of vectors, one for each formula.
coef.straussR = function(object, summary = mean, show_effects = FALSE,
                         unlist = TRUE) {
  stan_data = rstan::extract(object$stan_object)

  coefs_names = get_domain(object$formula, object$priors, object$data)

  coefs = if(unlist) lapply(coefs_names, function(coef) {
    stats::setNames(rep(0, length(coef)), names(coef))
  }) else lapply(coefs_names, function(coef) {
    stats::setNames(vector("list", length(coef)), names(coef))
  })

  unbounded_index = 0
  positive_index  = 0
  unit_index      = 0

  for(j in 1:length(coefs_names)) {
    for(i in 1:length(coefs_names[[j]])) {
      if(coefs_names[[j]][i]== "unbounded") {
        unbounded_index = unbounded_index + 1
        coefs[[j]][[i]] = summary(stan_data$beta_unbounded[, unbounded_index])
      } else if (coefs_names[[j]][i] == "positive") {
        positive_index = positive_index + 1
        coefs[[j]][[i]]  = summary(stan_data$beta_positive[, positive_index])
      } else if (coefs_names[[j]][i] == "unit") {
        unit_index = unit_index + 1
        coefs[[j]][[i]] = summary(stan_data$beta_unit[, unit_index])
      }
    }
  }

  coefs
}

print.straussR <- function(x, digits = max(3L, getOption("digits") - 3L),
                           summary = mean,
                           ...) {

  cat("# ======================================================= # \n")
  cat("# straussR object \n")
  cat("# ======================================================= # \n \n")
  summary_name = substitute(summary)
  cat("Formula:\n  ")
  form = x$formula
  environment(form) = NULL
  class(form) = NULL
  print.default(form)
  cat("\n")
  cat("Priors: \n")
  for(i in 1:length(priors)) {
    name = names(priors)[i]
    cat(" ", name)
    cat(": \n")
    for(prior_point in priors[[i]]) {
      form = prior_point
      environment(form) = NULL
      class(form) = NULL
      cat("    ")
      print.default(form)
    }
  }

  cat("\n")

  coefs = coef(x, summary = summary, show_effects = FALSE)

  if(length(coefs) != 0) {
    cat(paste0("Summary of coefficients (", deparse(summary_name), ")"))
    cat("\n")
    for(i in 1:length(coefs)) {
      cat("  ")
      cat(names(coefs)[i])
      cat(": \n")
      print.default(c("", format(coefs[[i]], digits = digits)),
                    print.gap = 1, quote = FALSE)
    }
  }

  invisible(x)
}

confint.straussR = function(object, parm, level = 0.95) {
  if(!missing(parm))
    stop(paste0("The option 'parm' is not implemented; use `[[` or `$` on the ",
                "returned list instead."))
  alpha = (1 - level)/2
  coef(x,
       summary = function(x) quantile(x, c(alpha, 1 - alpha)),
       unlist = FALSE)

}


hist.straussR = function(x, parameter, coef, ...) {

  coefs_names = get_domain(x$formula, x$priors, x$data)

  if(!(parameter %in% names(coefs_names)))
    stop("The parameter is not in the straussR object.")
  if(!(coef %in% names(coefs_names[[parameter]])))
    stop("The coefficient does not belong to the parameter.")

  domain = (coefs_names[[parameter]])[names(coefs_names[[parameter]]) == coef]
  unlist(coefs_names)
}



#' Get the domains
get_domain = function(formula, priors, data = NULL) {

  rhs = formula[[3]]
  formulas = lapply(X   = rhs[2:length(formula[[3]])],
                    FUN = as.formula,
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

  model = model.matrix(as.formula(formula_str), data)
  index_matrix = matrix(0, nrow = length(terms), ncol = ncol(model))

  lapply(priors, function(prior) {
    massage_priors(prior, data = data)$prior_domain
  })
}

