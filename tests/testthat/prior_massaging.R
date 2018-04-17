context("prior_massaging")

stuff_data = data.frame(z = 1:26, x = LETTERS, y = 1:26)
prior_1 = x ~ gamma(1, 1)
prior_2 = y ~ normal(0, 1)

testthat::expect_equal(length(massage_prior(stuff_prior_1, data = stuff_data)),
                       25)
testthat::expect_equal(length(massage_prior(stuff_prior_2, data = stuff_data)),
                       1)

testthat::expect_equal(length(massage_prior(stuff_prior_1, data = NULL)),
                       1)
testthat::expect_equal(length(massage_prior(stuff_prior_2, data = NULL)),
                       1)