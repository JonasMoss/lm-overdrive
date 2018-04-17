formula = z ~ gamma(log(mean) ~ 1 + year_factor,
                    sd        ~ 1,
                    logit(p)  ~ 1 + year_factor + M_scaled)

priors = list(mean = list((Intercept) ~ normal(0, 1),
                          year_factor ~ normal(0, 1)),
              sd   = list((Intercept) ~ gamma(1, 1)),
              p    = list((Intercept) ~ normal(0, 1),
                          year_factor ~ normal(0, 1),
                          M_scaled    ~ normal(0, 1)))

straussR(formula = formula, data = motyl_data, priors = priors, chains = 1,
         control = list(adapt_delta = 0.99)) ->
  motyl_year