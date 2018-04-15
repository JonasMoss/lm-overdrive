straussR = rstan::stan_model(file       = "src/stan_files/straussR.stan",
                             model_name = "straussR")


#' strauss
#'
#' Linear regression with STAN with some options.
#'
#' @param formula The formula.
#' @param data The data.
#' @param priors List of formulas. Priors for the coefficients for
#' \code{p}.
#' @return A \code{STAN} object.

strauss = function(formula, data, priors = NULL, ...) {

  dots = alist2(...)

  if(missing(data)) data = NULL
  model = family_formula_to_list2(formula)
  model_list = model_matrix(model$rhs_list, priors = priors, data = data)
  priors_massage = prior_list_massage(priors)


  sdata = list()

  sdata$M            = data$M
  sdata$lower_bounds = data$lower
  sdata$upper_bounds = data$upper
  sdata$dist_indices = data$dist_indices


  sdata$MAX_PAR = MP
  sdata$N       = nrow(model_list$X)
  sdata$P       = ncol(model_list$unbounded_indices)
  sdata$Q       = nrow(model_list$unbounded_indices)
  sdata$family  = model$family

  sdata$Z       = model.frame(model$response, data = data)[, 1]
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

  if(is.null(dots$init)) {
    no_unbounded = sum(sdata$no_unbounded)
    no_positive = sum(sdata$no_positive)
    no_unit = sum(sdata$no_unit)
    init = function(chain_id = 1) {
      list(thetas = rep(0.1, sdata$N))
    }
    dots$init = init
  }
  # Done with data insertion, time for sampling!
  stan_fit = do_call(rstan::sampling, .args = dots,
                     object = straussR,
                     data   = sdata)
  stan_fit
}


# Simulation of folded normals.

set.seed(1337)
N = 100
M = sample(N, 20:80, replace = TRUE)
x1 = rexp(N, 5)
p = rbinom(N, 1, 0.7)
mean_ = 0.1
sd_   = 0.1
thetas = rgamma(N, mean_^2/sd_^2, mean_/sd_^2)
lower = rep(1.96, N)
upper = rep(0, N)
z = rnorm(N, sqrt(M)*thetas, 1)*(1 - p) +
    truncnorm::rtruncnorm(N, mean = sqrt(M)*thetas, sd = 1, a = lower)*p

dist_indices = rep(3, N)
#dist_indices[z < 1.96] = 7


data = data.frame(z = z, x1 = x1, lower = lower, upper = upper,
                     dist_indices = dist_indices, M = M)

formula = z ~ gamma(mean ~ 1, sd ~ 1, p ~ 1)
priors = list(mean = list((Intercept) ~ gamma(1, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p    = list((Intercept) ~ beta(1, 1)))


strauss(formula = formula, priors = priors, data = data, chains = 1) -> mods