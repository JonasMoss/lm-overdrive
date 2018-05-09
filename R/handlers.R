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
#' @return A list of vectors, one for each formula.
coef.straussR = function(object, summary = mean, show_effects = FALSE) {
  stan_data = rstan::extract(object$stan_object)

  coefs_names = get_domain(object$formula, object$priors, object$data)
  coefs = lapply(coefs_names, function(coef) {
    setNames(rep(0, length(coef)), names(coef))
    })

  unbounded_index = 0
  positive_index  = 0
  unit_index      = 0

  for(j in 1:length(coefs)) {
    for(i in 1:length(coefs[[j]])) {
      if(coefs_names[[j]][i]== "unbounded") {
        unbounded_index = unbounded_index + 1
        coefs[[j]][i] = summary(stan_data$beta_unbounded[, unbounded_index])
      } else if (coefs_names[[j]][i] == "positive") {
        positive_index = positive_index + 1
        coefs[[j]][i]  = summary(stan_data$beta_positive[, positive_index])
      } else if (coefs_names[[j]][i] == "unit") {
        unit_index = unit_index + 1
        coefs[[j]][i] = summary(stan_data$beta_unit[, unit_index])
      }
    }
  }

  coefs
}

print.straussR <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  x
  cat("\nCall:  ",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  if(length(coef(x))) {
    cat("Coefficients")
    if(is.character(co <- x$contrasts))
      cat("  [contrasts: ",
          apply(cbind(names(co),co), 1L, paste, collapse = "="), "]")
    cat(":\n")
    print.default(format(x$coefficients, digits = digits),
                  print.gap = 2, quote = FALSE)
  } else cat("No coefficients\n\n")
  cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ",
      x$df.residual, "Residual\n")
  if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep = "")
  cat("Null Deviance:	   ",	format(signif(x$null.deviance, digits)),
      "\nResidual Deviance:", format(signif(x$deviance, digits)),
      "\tAIC:", format(signif(x$aic, digits)))
  cat("\n")
  invisible(x)
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

