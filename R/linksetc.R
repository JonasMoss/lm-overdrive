## Implement a 'map' structure with multiple keys.
## Each element has an ordinary name as in a list, but several keys it
## can be accessed by. Each element has more info, of course.
## Each element has an element named 'key' or something, which is searched.

is_formula = function(obj) inherits(obj, "formula")

#' Creates a formula by hand.
#'
#' Taken from the package 'lazyeval'.
#'
new_formula = function (rhs, lhs = NULL, env = parent.frame()) {
  if (!is.environment(env)) {
    stop("`env` must be an environment", call. = FALSE)
  }
  if (is.null(lhs)) {
    f <- call("~", rhs)
  }
  else {
    f <- call("~", lhs, rhs)
  }
  structure(f, class = "formula", .Environment = env)
}


matchy = function(family) match.arg(family, names(family_list))



is_two_sided = function(formula) {
  formula_str = paste(as.character(formula)[2], "~",
                      as.character(formula)[3])
  msg = paste0("The formula must be two-sided, got '", formula_str, "'.")
  assertthat::assert_that(length(formula) == 3, msg = msg)
}

#' Convert a \code{straussR} formula
#'
#' Convert a \code{straussR} formula into a list containing the response, the
#' formula and link for the mean and standard deviation, and the prior for
#' the additional parameter(s). [Not implemented yet.]
#'
#' @param formula A formula object.
#' @return A list.

family_to_stan = function(formula) {

  formula_str = paste(as.character(formula)[2], "~",
                      as.character(formula)[3])
  msg = paste0("Expected formula, got object of class '", class(formula),
               "': ", formula_str, ".")
  assertthat::assert_that(is_formula(formula), msg = msg)
  is_two_sided(formula)

  env = environment(formula)

  lhs = formula[[3]]

  family = match.arg(deparse(lhs[[1]]), names(family_list))

  arg_length = 2 + length(family_list[[family]]$extra_parameters)
  assertthat::assert_that(arg_length == length(lhs) - 1)

  ## Now we handle the formula and link of the first argument.

  mean_formula = as.formula(lhs[[2]], env = env)
  sd_formula = as.formula(lhs[[3]], env = env)

  ## Now we handle potential additional arguments.
  # if(arg_length > 2) {
  #   for(i in (3:arg_length)) {
  #     current_formula = substituted[[i + 1]]
  #   }
  # }

  c(list(response = new_formula(rhs = 0, lhs = formula[[2]], env = env),
         family = family), link_formula_to_list(sd_formula, type = "sd"),
                           link_formula_to_list(mean_formula, type = "mean"))
}

#' Link-formula to link name and formula
#'
#' @param formula
#' @return List of link name and formula

link_formula_to_list = function(formula, type = "p") {
  formula_str = paste(as.character(formula)[2], "~",
                      as.character(formula)[3])
  msg = paste0("Expected formula, got object of class '", class(formula),
               "': ", formula_str, ".")
  assertthat::assert_that(is_formula(formula), msg = msg)
  is_two_sided(formula)

  env = environment(formula)

  is_two_sided(formula)

  return_list = list()
  return_list[[paste0(type, "_link")]] = link_to_stan(formula, type = type)
  return_list[[paste0(type, "_formula")]] = new_formula(formula[[3]], env = env)
  return_list
}

link_formula_to_list(log(p) ~ dist + mpg, type = "p")

formula = mpg ~ gumbel(log(mean) ~ cyl + disp + hp,
                       1/sd      ~ hp)

family_to_stan(formula)

do_fun = function(formula, data, p = NULL, mean_priors = NULL,
                  sd_priors = NULL, p_priors = NULL) {

  if(missing(data)) data = NULL
  model_list = family_to_stan(formula)
  if(!is.null(p)) p_list = link_formula_to_list(p, type = "p")

  y = unlist(model.frame(model_list$response, data))
  x_mean = model.matrix(model_list$mean_formula, data)
  x_sd   = model.matrix(model_list$sd_formula, data)
  x_p    = model.matrix(p_list$p_formula, data)
  list(y, x_mean, x_sd, x_p)
}

formula = mpg ~ gumbel(log(mean) ~ cyl + disp + hp,
                       1/sd      ~ hp)
p = p ~ mpg + disp

do_fun(formula, mtcars, p = p)



family_to_stan(theta ~ skew_normal(log(mean) ~ c,
                                   1/sd      ~ b,
                                   alpha     ~ normal(0, 1)))


is_formula(theta ~ skew_normal(log(mean) ~ c,
                                   1/sd      ~ b,
                                   alpha     ~ normal(0, 1)))

substitute(skew_normal(log(mean) ~ c,
                       1/sd      ~ b,
                       alpha     ~ normal(0, 1))) -> substituted


#' Matches a formula with a link.
#'
#' @param formula A formula object.
#' @param type The string used as a function argument.
#' @return A string, identifying a link in the 'links' list.

link_to_stan = function(formula, type = "mean") {

  formula_text = gsub(type, "x", deparse(attr(terms(formula), "variables")[[2]]))
  index = which(sapply(links, function(x) formula_text  %in% x$keys))
  link_name = names(links)[index]
  msg = paste0("The link supplied for '", type, "' is not supported: ",
               deparse(attr(terms(formula), "variables")[[2]]))
  assertthat::assert_that(length(link_name) != 0, msg = msg)
  link_name
}

form = log(mean) ~ a + b
form = 1/mean^2 ~ a + b
#form = mean ~ a + b
formula = form
link_to_stan(formula)

# link_to_stan = function(formula, type = "mean") {
#   response = attr(terms(form), "variables")[[2]]
#   link = NA
#   if(length(response) == 1) {
#     link = "identity"
#     variable = deparse(response)
#   } else if(length(response) == 2) {
#     link = deparse(response[[1]])
#     variable = deparse(response[[2]])
#   } else if(length(response) == 3) {
#     if(deparse(response[[1]]) == "1" & deparse(response[[2]]) == "/") link = "inverse"
#     variable = deparse(response[[3]])
#   }
#   # stop(paste0("The right hand side '", deparse(response), "'
#   # is not supported for the type '", type, "'."))
#
#
#   # text = paste0("quote(", deparse(substitute(family)),")")
#   # eval(parse(text = text))
# }