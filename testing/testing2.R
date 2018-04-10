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




## =============================================================================
## Testing check_signature
## =============================================================================
formals = alist(mu = , sigma = )

check_signature(formals, mu = 0, sigma = 1)
check_signature(formals, mu = 0, 1)
check_signature(formals, sigma = 1, mu = 0)
check_signature(formals, sigma = 1, 0)
check_signature(formals, 0, 1)
check_signature(formals, 1, mu = 0)
check_signature(formals, 0, sigma = 1)
check_signature(formals, m = 0, s = 1)

check_signature(formals, mu = 0)
check_signature(formals, sigma = 0)