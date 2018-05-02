context("match_formals")

formals = alist(mu = , sigma = )
args = c(mu = 0, sigma = 1)

expect_equal(match_formals(formals, mu = 0, sigma = 1), args)
expect_equal(match_formals(formals, mu = 0, sigma = 1), args)
expect_equal(match_formals(formals, mu = 0, 1), args)
expect_equal(match_formals(formals, sigma = 1, mu = 0), args)
expect_equal(match_formals(formals, sigma = 1, 0), args)
expect_equal(match_formals(formals, 0, 1), args)
expect_equal(match_formals(formals, 1, mu = 0), args)
expect_equal(match_formals(formals, 0, sigma = 1), args)
expect_equal(match_formals(formals, m = 0, s = 1), args)

expect_error(match_formals(formals, mu = 0))
expect_error(match_formals(formals, sigma = 0))