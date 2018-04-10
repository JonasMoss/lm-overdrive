context("prior_list_manipulations")

formula_sn_1 = disp ~ skew_normal(xi = 1, omega = 2, alpha = 3)
formula_sn_2 = disp ~ skew_normal(omega = 2, alpha = 3, 1)
formula_sn_3 = disp ~ skew_normal(xi = 1, 2, 3)
formula_sn_4 = disp ~ skew_normal(1, alpha = 3, 2)
formula_sn_5 = disp ~ skew_normal(xi = 1, znomega = 2, alpha = 3)
formula_sn_6 = disp ~ skew_normal(xi = 1, omega = 2)

expect_equal(prior_formula_to_list(formula_sn_1),
             prior_formula_to_list(formula_sn_2))
expect_equal(prior_formula_to_list(formula_sn_2),
             prior_formula_to_list(formula_sn_3))
expect_equal(prior_formula_to_list(formula_sn_3),
             prior_formula_to_list(formula_sn_4))
expect_equal(prior_formula_to_list(formula_sn_4),
             prior_formula_to_list(formula_sn_1))

expect_error(prior_formula_to_list(formula_sn_5))
expect_error(prior_formula_to_list(formula_sn_6))

#
# priors = list(
#   disp ~ skew_normal(xi = 1, omega = 2, alpha = 3),
#   mpg ~ normal(sigma = 1, mu = 3),
#   wt ~ student_t(nu = 1, mu = 4, alpha = 4))
#
# prior_massage(priors)