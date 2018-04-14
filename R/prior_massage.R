#' Massages priors into triple matrix form.
#'
#' @param prior_list A list of priors.
#' @return A list of nine matrices.

prior_list_massage = function(priors) {
  priors_massage  = lapply(priors, prior_massage)

  max_length_unbounded = max(sapply(priors_massage, function(elem) {
    length(elem$prior_domain[elem$prior_domain == "unbounded"])
  }))

  max_length_positive = max(sapply(priors_massage, function(elem) {
    length(elem$prior_domain[elem$prior_domain == "positive"])
  }))

  max_length_unit = max(sapply(priors_massage, function(elem) {
    length(elem$prior_domain[elem$prior_domain == "unit"])
  }))

  unbounded_prior = array(data = 0, dim = c(max_length_unbounded, MP, length(priors)))
  positive_prior  = array(data = 0, dim = c(max_length_positive, MP, length(priors)))
  unit_prior      = array(data = 0, dim = c(max_length_unit, MP, length(priors)))

  unbounded_prior_types = array(data = 0, dim = c(max_length_unbounded, length(priors)))
  positive_prior_types  = array(data = 0, dim = c(max_length_positive, length(priors)))
  unit_prior_types      = array(data = 0, dim = c(max_length_unit, length(priors)))


  unbounded_prior_types = array(data = 0, dim = c(length(priors), max_length_unbounded))
  positive_prior_types  = array(data = 0, dim = c(length(priors), max_length_positive))
  unit_prior_types      = array(data = 0, dim = c(length(priors), max_length_unit))


  index = 1
  for(prior in priors_massage) {

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
  list(unbounded_prior       = unbounded_prior,
       positive_prior        = positive_prior,
       unit_prior            = unit_prior,
       unbounded_prior_types = unbounded_prior_types,
       positive_prior_types  = positive_prior_types,
       unit_prior_types      = unit_prior_types)
}