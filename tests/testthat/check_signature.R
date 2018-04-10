context("check_signature")

formals = alist(mu = , sigma = )
args = c(mu = 0, sigma = 1)

expect_equal(check_signature(formals, mu = 0, sigma = 1), args)
expect_equal(ccheck_signature(formals, mu = 0, sigma = 1), args)
expect_equal(check_signature(formals, mu = 0, 1), args)
expect_equal(check_signature(formals, sigma = 1, mu = 0), args)
expect_equal(check_signature(formals, sigma = 1, 0), args)
expect_equal(check_signature(formals, 0, 1), args)
expect_equal(check_signature(formals, 1, mu = 0), args)
expect_equal(check_signature(formals, 0, sigma = 1), args)
expect_equal(check_signature(formals, m = 0, s = 1), args)

expect_error(check_signature(formals, mu = 0))
expect_error(check_signature(formals, sigma = 0))