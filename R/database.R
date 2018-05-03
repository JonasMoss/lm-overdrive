## This file contains constants used in the package. Most important is the
## .database environment of supported priors, families, and links. It can also
## be used as a guide to the (forced) integer-coding in STAN.

# Maximal number of parameters in a distribution. A constant.
MAX_PAR = 6

## The .database environment must be initialized. Described above and below.
.database = new.env()

## A description of the likelihoods involved.
tmp_names = c(
  ## Mixtures of truncated and non-truncated fnormals and normals.
  "Mixture, folded normal with lower bound",
  "Mixture, folded normal with upper bound",
  "Mixture, folded normal with double bound",
  "Mixture, folded normal with inner bound",

  "Mixture, normal with upper bound",
  "Mixture, normal with lower bound",
  "Mixture, normal with double bound",
  "Mixture, normal with inner bound",

  ## fnormals and normals that affect p.
  "Folded normal, affects p",
  "Normal, affects p",

  ## Truncated and non-truncated fnormals and normals that affect p.
  "Lower truncated folded normal, affects p",
  "Upper truncated folded normal, affects p",
  "Doubly truncated folded normal, affects p",
  "Inner truncated folded normal, affects p",

  "Lower truncated normal, affects p",
  "Upper truncated normal, affects p",
  "Doubly truncated normal, affects p",
  "Inner truncated normal, affects p",

  ## fnormals and normals that don't affect p.
  "Folded normal, does not affect p",
  "Normal, does not affect p",

  ## Truncated and non-truncated fnormals and normals that don't affect p.
  "Lower truncated folded normal, does not affect p",
  "Upper truncated folded normal, does not affect p",
  "Doubly truncated folded normal, does not affect p",
  "Inner truncated folded normal, does not affect p",

  "Lower truncated normal, does not affect p",
  "Upper truncated normal, does not affect p",
  "Doubly truncated normal, does not affect p",
  "Inner truncated normal, does not affect p"
)

.database$likelihood = stats::setNames(1:length(tmp_names), tmp_names)
rm(tmp_names)

## Is p needed for the likelihoods?
.database$includes_p = c(rep(TRUE, 20), rep(FALSE, 8))

## The database of families for _effect size distributions_. These are not the
## same as the families for the priors or the likelihood

.database$families = list(
  fixed = list(integer = 0,
               domain = "unbounded",
               parameters = list(mean = "unbounded")),
  normal = list(integer = 1,
                domain  = "unbounded",
                parameters = list(mean = "unbounded",
                                  sd   = "positive")),

  gumbel = list(integer = 2,
                domain  = "unbounded",
                parameters = list(mean = "unbounded",
                                  sd   = "positive")),

  skew_normal = list(integer = 3,
                     domain  = "unbounded",
                     parameters = list(mean  = "unbounded",
                                       sd    = "positive",
                                       alpha = "unbounded")),
  gamma = list(integer = 4,
               domain  = "positive",
               parameters = list(mean = "positive",
                                 sd   = "positive")),

  lognormal = list(integer = 5,
                   domain  = "positive",
                   parameters = list(mean = "positive",
                                     sd   = "positive")),

  inverse_gaussian = list(integer = 6,
                          domain  = "positive",
                          parameters = list(mean = "positive",
                                            sd   = "positive")),

  beta = list(integer = 7,
              domain  = "unit",
              parameters = list(mean      = "unit",
                                precision = "unit")),

  exp_mod_normal = list(integer = 8,
                        domain  = "unbounded",
                        parameters = list(mu     = "unbounded",
                                          sigma  = "positive",
                                          lambda = "positive")),
  fnormal = list(integer = 9,
                 domain  = "positive",
                 parameters = list(mu    = "unbounded",
                                   sigma = "positive")),
  truncnormal = list(integer = 10,
                 domain  = "positive",
                 parameters = list(mu    = "unbounded",
                                   sigma = "positive"))

)

## The database of families for _priors_. These are not the same as the families
## for the effect size distributions.
.database$priors = list(
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


.database$links = list(
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