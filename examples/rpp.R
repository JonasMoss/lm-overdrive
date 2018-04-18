readxl::read_excel("data/rpp.xlsx") -> rpp

## =============================================================================
## Cleaning the data:
## =============================================================================

sapply(rpp$Test.statistic.O, strsplit, split = " = ") -> test.O.split
sapply(test.O.split, function(obj) obj[1]) -> test.O.name
as.numeric(sapply(test.O.split, function(obj) obj[2])) -> test.O.value

sapply(rpp$Test.statistic.R, strsplit, split = " = ") -> test.R.split
sapply(test.R.split, function(obj) obj[1]) -> test.R.name
as.numeric(sapply(test.R.split, function(obj) obj[2])) -> test.R.value

data = data.frame(z.original        = abs(test.O.value),
                  z.name.original   = test.O.name,
                  z.replicated      = abs(test.R.value),
                  z.name.replicated = test.R.name,
                  df.original       = rpp$T.df2.O,
                  df.replicated     = rpp$T.df2.R)

data %>%
  dplyr::filter(stringr::str_detect(z.name.original, "^F\\(1") |
                  stringr::str_detect(z.name.original, "^t")) ->
  data

data %>%
  dplyr::mutate(z.original = ifelse(stringr::str_detect(z.name.original, "^F\\(1"),
                             sqrt(z.original),
                             z.original)) ->
  data

data %>%
  dplyr::mutate(z.replicated = ifelse(stringr::str_detect(z.name.replicated, "^F\\(1"),
                                    sqrt(z.replicated),
                                    z.replicated)) ->
  data

data$z.name.original = NULL
data$z.name.replicated = NULL


rpp_data = data.frame(
  z            = data$z.original,
  M            = data$df.original,
  d.original   = data$z.original/sqrt(data$df.original),
  d.replicated = data$z.replicated/sqrt(data$df.replicated),
  lower        = qt(0.975, data$df.original),
  upper        = qt(0, data$df.original),
  dist_indices = rep(1, length(data$df.original)))

rpp_data$dist_indices[data$z < data$lower] = 5
rpp_data = na.omit(data)

## =============================================================================
## Running an analysis:
## =============================================================================
formula = z ~ fnormal(mean ~ 1, sd ~ 1, p ~ 1)
priors = list(mean = list((Intercept) ~ gamma(1, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p    = list((Intercept) ~ beta(1, 1)))

straussR(formula = formula, data = rpp_data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.99)) -> rpp_model


thetas = colMeans(rstan::extract(rpp_model)$thetas_positive)
plot(rpp_data$d.original, thetas)
plot(rpp_data$d.original, rpp_data$d.replicated)
plot(thetas, rpp_data$d.replicated)

cor(na.omit(cbind(rpp_data$d.original, rpp_data$d.replicated, thetas)))
mean(thetas)
mean(rpp_data$d.original, na.rm = TRUE)
mean(rpp_data$d.replicated, na.rm = TRUE)
