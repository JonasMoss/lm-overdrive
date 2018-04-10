formula = mpg ~ skew_normal(1/mean ~ disp + wt,
                            log(sd) ~ drat + cyl,
                            alpha ~ normal(0, 1))

family_formula_to_list(formula)



do_fun = function(formula, data, p = NULL, mean_priors = NULL,
                  sd_priors = NULL, p_priors = NULL) {

  if(missing(data)) data = NULL
  model_list = family_formula_to_list(formula)
  if(!is.null(p)) p_list = formula_to_list(p, type = "p")

  y = unlist(model.frame(model_list$response, data))
  x_mean = model.matrix(model_list$mean_formula, data)
  x_sd   = model.matrix(model_list$sd_formula, data)
  x_p    = model.matrix(p_list$p_formula, data)

  list(y = y, x_mean = x_mean, x_sd = x_sd, x_p, x_p)
}


#' Check the signature
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
