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
                  df.replicated     = rpp$T.df2.R
                  )

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
  M.replicated = data$df.replicated,
  d.original   = data$z.original/sqrt(data$df.original),
  d.replicated = data$z.replicated/sqrt(data$df.replicated),
  lower        = qt(0.975, data$df.original),
  upper        = qt(0, data$df.original),
  dist_indices = rep(1, length(data$df.original)))

rpp_data$dist_indices[data$z < data$lower] = 5
rpp_data$M_scaled = scale(log(rpp_data$M))
rpp_data = na.omit(rpp_data)

## =============================================================================
## Running an analysis:
## =============================================================================

formula = z ~ fnormal(mean ~ 1, sd ~ 1, probit(p) ~ 1 + M_scaled)
priors = list(mean = list((Intercept) ~ gamma(1, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p    = list((Intercept) ~ normal(0, 1),
                          M_scaled ~ normal(0, 1)))


formula = z ~ fnormal(mean ~ 1, sd ~ 1, probit(p) ~ 1)
priors = list(mean = list((Intercept) ~ gamma(1, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p    = list((Intercept) ~ normal(0, 1)))


set.sees(313)
straussR(formula = formula, data = rpp_data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.99)) -> rpp_model


thetas = colMeans(rstan::extract(rpp_model)$thetas_positive)
theta = rstan::extract(rpp_model)$thetas_positive
plot(rpp_data$d.original, thetas)
plot(rpp_data$d.original, rpp_data$d.replicated)
plot(thetas, rpp_data$d.replicated)

cor(na.omit(cbind(rpp_data$d.original, rpp_data$d.replicated, thetas)))
mean(thetas)
mean(rpp_data$d.original, na.rm = TRUE)
mean(rpp_data$d.replicated, na.rm = TRUE)

mean((rpp_data$d.original - rpp_data$d.replicated)^2, na.rm = TRUE)
mean((thetas - rpp_data$d.replicated)^2, na.rm = TRUE)

theta = rstan::extract(rpp_model)$thetas_positive
for(i in 1:ncol(theta)) {
  theta[ , i] = theta[ , i] + rnorm(nrow(theta), 0, 1/sqrt(rpp_data$M.replicated[i]))
}



thetas_05 = pmax(apply(theta, 2, quantile, 0.05), 0)
thetas_50 = apply(theta, 2, quantile, 0.5)
thetas_95 = apply(theta, 2, quantile, 0.95)

orders = order(rpp_data$d.replicated)
orders = order(thetas_50)
plot(thetas_50[orders], type = "l", col = "blue", ylim = c(-0.01, 2),
     bty = "l", main = "Prediction bounds", xlab = "Index", ylab = "d")
points(thetas_95[orders], type = "l", col = "red")
points(thetas_05[orders], type = "l", col = "red")
points(rpp_data$d.replicated[orders], col = "black", pch = 20)
points(rpp_data$d.original[orders], col = "green", pch = 20)


