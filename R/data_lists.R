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
              extra_parameters = NULL))

links = list(
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
                  domian  = "unbounded",
                  parameters = list(mu     = "unbounded",
                                    sigma  = "positive")),

  "exp_mod_normal" = list(integer = 101,
                          domian  = "unbounded",
                          parameters = list(mu     = "unbounded",
                                            sigma  = "positive",
                                            lambda = "positive")),

  "skew_normal" = list(integer = 102,
                       domian  = "unbounded",
                       parameters = list(xi    = "unbounded",
                                         omega = "positive",
                                         alpha = "unbounded")),

  "student_t" = list(integer = 103,
                     domian  = "unbounded",
                     parameters = list(nu    = "positive",
                                       mu    = "unbounded",
                                       alpha = "positive")),

  "cauchy" = list(integer = 104,
                  domian  = "unbounded",
                  parameters = list(mu    = "unbounded",
                                    sigma = "positive")),

  "double_exponential" = list(integer = 105,
                              domian  = "unbounded",
                              parameters = list(mu    = "unbounded",
                                                sigma = "positive")),

  "logistic" = list(integer = 106,
                    domian  = "unbounded",
                    parameters = list(mu    = "unbounded",
                                      sigma = "positive")),

  "gumbel" = list(integer = 107,
                  domian  = "unbounded",
                  parameters = list(mu    = "unbounded",
                                    sigma = "positive")),

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
  "beta"                  = 500
)