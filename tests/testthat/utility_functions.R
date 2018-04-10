context("manipulate_calls")

args_lazy = alist(omega = 2, alpha = , 1)
call_lazy = quote(skew_normal(omega = 2, alpha = , 1))

args_eager = alist(omega = 2, alpha = 3, 1)
call_eager = quote(skew_normal(omega = 2, alpha = 3, 1))

fun = quote(skew_normal)

expect_equal(extract_arguments(call_lazy), args_lazy)
expect_equal(extract_arguments(call_eager), args_eager)
expect_equal(extract_function(call_lazy), fun)
expect_equal(extract_function(call_eager), fun)