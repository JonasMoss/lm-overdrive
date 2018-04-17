N = 10^5

generate_re_data(N      = N,
                 n      = rep(1, N),
                 theta  = rnorm,
                 distribution = "folded_normal",
                 class  = "or",
                 lower  = 3,
                 upper  = 2,
                 mean   = 0,
                 sd     = 1,
                 p_true = 1) ->
  test1

hist(test1$z, freq = FALSE, breaks = 100)

strauss(data                = test1,
        effect_distribution = "gamma",
        type                = 0,
        chains              = 1,
        control             = list(adapt_delta = 0.99)) ->
  fit_test1



plot(log(fit_test1$stan_data$n_abs_b),
     log(colMeans(extract(fit_test1$fit)$thetas_abs_b)))
abline(lm(log(colMeans(extract(fit_test1$fit)$thetas_abs_b)) ~
          log(fit_test1$stan_data$n_abs_b)), col = "black")

points(log(fit_test1$stan_data$n_abs_b),
       log(colMeans(extract(fit_test1$fit)$thetas_abs_b)), col = "blue")
abline(lm(log(colMeans(extract(fit_test1$fit)$thetas_abs_b)) ~
          log(fit_test1$stan_data$n_abs_b)), col = "blue")

plot(log(fit_test1$stan_data$n_abs_b),
     log(fit_test1$stan_data$d_abs_b))

cor(log(fit_test1$stan_data$n_abs_b),
    log(colMeans(extract(fit_test1$fit)$thetas_abs_b)))

cor(log(fit_test1$stan_data$n_abs_b),
    log(fit_test1$stan_data$d_b))


lm(log(colMeans(extract(fit_test1$fit)$thetas_abs_b)) ~
   log(fit_test1$stan_data$n_abs_b)) %>%
  summary