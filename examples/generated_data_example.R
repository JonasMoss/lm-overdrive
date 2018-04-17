N = 2000
snifmar_nmre_generate(N          = N,
                      n          = sample(20:60, N, replace = TRUE),
                      theta_true = -0.1,
                      sigma_true = 0.1,
                      p_true     = 0.6,
                      alpha      = qnorm(0.95)) ->
  data

snifmar_normal_mixture(data$z, data$n, data$alpha, sigma0 = 0.1)
