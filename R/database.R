# This is a database of supported priors, families, and links.


MP = 6 # Maximal number of parameters in a distribution. A constant.

family_list = list(
  normal = list(integer = 1,
                domain  = "unbounded",
                mean    = "unbounded",
                sd      = "positive",
                extra_parameters = NULL),

  gumbel = list(integer = 2,
                domain  = "unbounded",
                mean    = "unbounded",
                sd      = "positive",
                extra_parameters = NULL),

  skew_normal = list(integer = 3,
                     domain  = "unbounded",
                     mean    = "unbounded",
                     sd      = "positive",
                     extra_parameters = list(alpha = "unbounded")),

  gamma = list(integer = 4,
               domain  = "positive",
               mean    = "positive",
               sd      = "positive",
               extra_parameters = NULL),

  lognormal = list(integer = 5,
                   domain  = "positive",
                   mean    = "positive",
                   sd      = "positive",
                   extra_parameters = NULL),

  inverse_gaussian = list(integer = 6,
                          domain  = "positive",
                          mean    = "positive",
                          sd      = "positive",
                          extra_parameters = NULL),

  beta = list(integer = 7,
              domain  = "unit",
              mean    = "unit",
              sd      = "unit",
              extra_parameters = NULL),

  exp_mod_normal = list(integer = 8,
                        domain  = "unbounded",
                        mean    = "unbounded",
                        sd      = "positive",
                        extra_parameters = list(lambda = "positive")),

  fnormal = list(integer = 9,
                 domain  = "positive",
                 mean    = "unbounded",
                 sd      = "positive",
                 extra_parameters = NULL))

link_list = list(
  inverse = list(
    integer = 2,
    keys    = c("inverse(x)", "1/x"),
    domain  = "unbounded"),

  inverse_squared = list(
    integer = 3,
    keys    = c("inverse_squared(x)", "1/x^2"),
    domain  = "positive"),

  identity = list(
    integer = 1,
    keys    = c("identity(x)", "x"),
    domain  = "unbounded"),

  log = list(
    integer = 4,
    keys    = c("log(x)"),
    domain  = "positive"),

  sqrt = list(
    integer = 5,
    keys    = c("sqrt(x)"),
    domain  = "positive"),

  probit = list(
    integer = 6,
    keys    = c("probit(x)"),
    domain  = "unit"),

  cloglog = list(
    integer = 7,
    keys    = c("cloglog(x)"),
    domain  = "positive"),

  logit = list(
    integer = 8,
    keys    = c("logit(x)"),
    domain  = "positive"),

  cauchit = list(
    integer = 9,
    keys    = c("cauchit(x)"),
    domain  = "positive"))

prior_list = list(
  "normal" = list(integer = 100,
                  domain  = "unbounded",
                  parameters = list(mu     = "unbounded",
                                    sigma  = "positive")),

  "exp_mod_normal" = list(integer = 101,
                          domain  = "unbounded",
                          parameters = list(mu     = "unbounded",
                                            sigma  = "positive",
                                            lambda = "positive")),

  "skew_normal" = list(integer = 102,
                       domain  = "unbounded",
                       parameters = list(xi    = "unbounded",
                                         omega = "positive",
                                         alpha = "unbounded")),

  "student_t" = list(integer = 103,
                     domain  = "unbounded",
                     parameters = list(nu    = "positive",
                                       mu    = "unbounded",
                                       alpha = "positive")),

  "cauchy" = list(integer = 104,
                  domain  = "unbounded",
                  parameters = list(mu    = "unbounded",
                                    sigma = "positive")),

  "double_exponential" = list(integer = 105,
                              domain  = "unbounded",
                              parameters = list(mu    = "unbounded",
                                                sigma = "positive")),

  "logistic" = list(integer = 106,
                    domain  = "unbounded",
                    parameters = list(mu    = "unbounded",
                                      sigma = "positive")),

  "gumbel" = list(integer = 107,
                  domain  = "unbounded",
                  parameters = list(mu    = "unbounded",
                                    sigma = "positive")),

  "lognormal" = list(integer = 200,
                     domain  = "positive",
                     parameters = list(mu    = "unbounded",
                                       sigma = "positive")),

  "chi_square" = list(integer = 201,
                      domain  = "positive",
                      parameters = list(nu = "positive")),

  "inv_chi_square" = list(integer = 202,
                          domain  = "positive",
                          parameters = list(nu = "positive")),

  "scaled_inv_chi_square" = list(integer = 203,
                                 domain  = "positive",
                                 parameters = list(nu    = "positive",
                                                   sigma = "positive")),

  "exponential" = list(integer    = 204,
                       domain     = "positive",
                       parameters = list(beta = "positive")),

  "gamma" = list(integer = 205,
                 domain  = "positive",
                 parameters = list(alpha = "positive",
                                   beta  = "positive")),

  "inv_gamma" = list(integer = 206,
                     domain  = "positive",
                     parameters = list(alpha = "positive",
                                       beta  = "positive")),

  "weibull" = list(integer = 207,
                   domain  = "positive",
                   parameters = list(alpha = "positive",
                                     sigma = "positive")),

  "frechet" = list(integer = 208,
                   domain  = "positive",
                   parameters = list(alpha = "positive",
                                     sigma = "positive")),

  "rayleigh" = list(integer    = 300,
                    domain     = "positive",
                    parameters = list(sigma = "positive")),

  "wiener" = list(integer    = 301,
                  domain     = "positive",
                  parameters = list(alpha = "positive",
                                    tau   = "positive",
                                    beta  = "unit",
                                    sigma = "unbounded")),

  "beta" = list(integer = 500,
                domain  = "unit",
                parameters = list(alpha = "positive",
                                  beta  = "positive"))
)


link_list2 = list(
  inverse = list(
    integer = 2,
    keys    = list(quote(inverse), quote(`/`)),
    domain  = "unbounded"),

  inverse_squared = list(
    integer = 3,
    keys    = list(quote(inverse_squared)),
    domain  = "positive"),

  identity = list(
    integer = 1,
    keys    = list(quote(identity)),
    domain  = "unbounded"),

  log = list(
    integer = 4,
    keys    = list(quote(log)),
    domain  = "positive"),

  sqrt = list(
    integer = 5,
    keys    = list(quote(sqrt)),
    domain  = "positive"),

  probit = list(
    integer = 6,
    keys    = list(quote(probit)),
    domain  = "unit"),

  cloglog = list(
    integer = 7,
    keys    = list(quote(cloglog)),
    domain  = "positive"),

  logit = list(
    integer = 8,
    keys    = list(quote(logit)),
    domain  = "positive"),

  cauchit = list(
    integer = 9,
    keys    = list(quote(cauchit)),
    domain  = "positive"))