new_one = rstan::stan_model(file       = "src/stan_files/new_one.stan",
                            model_name = "testing")


#' Linear regression with STAN.
#'
#' Linear regression with STAN with some options.
#'
#' @param formula The formula.
#' @param data The data.
#' @param p_formula Formula for p.
#' @param mean_priors List of formulas. Priors for the coefficients for
#' \code{mean}.
#' @param sd_priors List of formulas. Priors for the coefficients for
#' \code{sd}.
#' @param p_priors List of formulas. Priors for the coefficients for
#' \code{p}.
#' @return A \code{STAN} object.

new_stanlm = function(formula, data, priors = NULL) {

  if(missing(data)) data = NULL
  model = family_formula_to_list2(formula)
  model_list = model_matrix(model$rhs_list, priors = priors, data = data)
  priors_massage = prior_list_massage(priors)


  sdata = list()

  sdata$MAX_PAR = MP
  sdata$N       = nrow(model_list$X)
  sdata$P       = ncol(model_list$unbounded_indices)
  sdata$Q       = nrow(model_list$unbounded_indices)
  sdata$family  = model$family

  sdata$Y       = model.frame(model$response, data = data)[, 1]
  sdata$X       = model_list$X

  sdata$no_unbounded = rowSums(model_list$unbounded_indices)
  sdata$no_positive  = rowSums(model_list$positive_indices)
  sdata$no_unit      = rowSums(model_list$unit_indices)

  sdata$link_types   = model$link_types

  sdata$unbounded_indices     = model_list$unbounded_indices
  sdata$unbounded_prior       = priors_massage$unbounded_prior
  sdata$unbounded_prior_types = priors_massage$unbounded_prior_types

  sdata$positive_indices     = model_list$positive_indices
  sdata$positive_prior       = priors_massage$positive_prior
  sdata$positive_prior_types = priors_massage$positive_prior_types

  sdata$unit_indices     = model_list$unit_indices
  sdata$unit_prior       = priors_massage$unit_prior
  sdata$unit_prior_types = priors_massage$unit_prior_types

  # Done with data insertion, time for sampling!
  stan_fit = do_call(rstan::sampling,
                     object = new_one,
                     data   = sdata)
  stan_fit
}

data = as.data.frame(scale(mtcars))


formula = mpg ~ normal(mean ~ disp + wt + cyl + hp, sd ~ 1, alpha ~ cyl)
priors = list(mean = list((Intercept) ~ normal(0, 1),
                          disp ~ normal(0, 1),
                          wt ~ normal(0, 1),
                          cyl ~ normal(0, 1),
                          hp ~ normal(0, 1)),
              sd = list((Intercept) ~ gamma(1, 1)),
              alpha = list((Intercept) ~ normal(0, 1),
                           cyl ~ skew_normal(1, 1, 1)))

lm(mpg ~ disp + wt + cyl + hp, data = data)
new_stanlm(formula, priors = priors, data = data)