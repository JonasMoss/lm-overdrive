stanlm_model = rstan::stan_model(file       = "src/stan_files/stanlm.stan",
                                 model_name = "stanlm")

family_to_extra_positive_parameters = function(family_name) {
  switch(family_name,
         "normal"      = 0,
         "gumbel"      = 0,
         "skew_normal" = 0,
         stop(paste0("Family '", family_name, "' not recognized.")))
}

family_to_extra_unbounded_parameters = function(family_name) {
  switch(family_name,
         "normal"      = 0,
         "gumbel"      = 0,
         "skew_normal" = 1,
         stop(paste0("Family '", family_name, "' not recognized.")))
}

family_to_extra_unit_parameters = function(family_name) {
  switch(family_name,
         "normal"      = 0,
         "gumbel"      = 0,
         "skew_normal" = 0,
         stop(paste0("Family '", family_name, "' not recognized.")))
}

prior_names_to_int = function(prior_name) {
  switch(prior_name,
         "normal"                = 100, # unbounded.
         "exp_mod_normal"        = 101,
         "skew_normal"           = 102,
         "student_t"             = 103,
         "cauchy"                = 104,
         "double_exponential"    = 105,
         "logistic"              = 106,
         "gumbel"                = 107,
         "lognormal"             = 200, # positive
         "chi_square"            = 201,
         "inv_chi_square"        = 202,
         "scaled_inv_chi_square" = 203,
         "exponential"           = 204,
         "gamma"                 = 205,
         "inv_gamma"             = 206,
         "weibull"               = 207,
         "frechet"               = 208,
         "rayleigh"              = 300, # non-negative
         "wiener"                = 301,
         "pareto"                = 400, # bounded below
         "pareto_type2"          = 401,
         "beta"                  = 500,
         stop(paste0("Prior '", prior_name, "' not recognized.")))
}

priors_change = function(priors) {
  prior_list = vector("list", length(priors))
  for(i in 1:length(priors)) {
    prior_list[[i]] = prior_formula_to_list(priors[[i]])
  }
  prior_list
}

prior_formula_to_list = function(form) {
  covariate = rownames(attr(terms(form), "factors"))[1]
  label = attr(terms(form), "term.labels")
  text = paste0("quote(", label,")")
  prior_quote = eval(parse(text = text))
  list(covariate  = covariate,
       prior      = deparse(prior_quote[[1]]),
       parameters = as.numeric(sapply(2:length(prior_quote), function(i) {
         deparse(prior_quote[[i]])
       })))
}

#' Transforms a list of priors to matrix form.
#'
#' @param priors List of priors.
#' @return A list containing a matrix of prior parameters and a vector of
#' prior types.
prior_massage = function(priors) {
  priors_list = priors_change(priors)
  mp = max(sapply(priors_list, function(prior) length(prior$parameters)))
  prior_parameters = matrix(0, ncol = mp, nrow = length(priors_list))

  for(i in 1:length(priors_list)) {
    prior_parameters[i, 1:length(priors_list[[i]]$parameters)] =
      priors_list[[i]]$parameters
  }
  rownames(prior_parameters) = sapply(priors_list,
                                      function(prior) prior$covariate)
  prior_types = sapply(priors_list, function(prior) {
    prior_names_to_int(prior$prior)
  })

  prior_types_names = sapply(priors_list, function(prior) prior$prior)
  prior_types = data.frame(type      = prior_types,
                           name      = prior_types_names)
  prior_types_covariate = sapply(priors_list, function(prior) prior$covariate)
  row.names(prior_types) = prior_types_covariate

  list(prior_parameters = prior_parameters,
       prior_types       = prior_types)
}

prior_massage_to_data = function(priors, names) {
  prior_list = prior_massage(priors)
  parameters = prior_list$prior_parameters
  types = prior_list$prior_types
  list(prior_parameters = parameters[!is.na(match(rownames(parameters), names)), ],
       prior_types      = types[!is.na(match(rownames(types), names)), ])
}