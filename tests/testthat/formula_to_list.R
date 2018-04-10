context("formula_to_list")

formula = log(mean) ~ a + b + c:d

expect_equal(formula_to_link(formula, type = "mean"), "log")
expect_equal(formula_to_list(formula, type = "mean")$mean_formula,
             rlang::new_formula(lhs = NULL, rhs = formula[[3]]))

formula = 1/sd ~ a + b + c:d

expect_equal(formula_to_link(formula, type = "sd"), "inverse")
expect_equal(formula_to_list(formula, type = "sd")$sd_formula,
             rlang::new_formula(lhs = NULL, rhs = formula[[3]]))


formula = mpg ~ skew_normal(1/mean ~ disp + wt,
                            log(sd) ~ drat + cyl,
                            alpha ~ normal(0, 1))

family_formula_to_list(formula)
