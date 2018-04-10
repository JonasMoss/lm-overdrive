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
