library("rstan")
test_model = rstan::stan_model(file       = "src/stan_files/testing.stan",
                               model_name = "testing")

new_one = rstan::stan_model(file       = "src/stan_files/new_one.stan",
                            model_name = "testing")


I = 1
J = 1
K = 1
L = 1
X = rgamma(I*J*K*L, 2, 1)
dim(X) = c(I, J, K, L)


stan_fit = do_call(rstan::sampling,
                   object = test_model,
                   data   = list(I = I, J = J, K = K, L = L, X = X)) -> mod