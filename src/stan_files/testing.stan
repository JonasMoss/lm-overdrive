functions {
// #include /functions/helpers.stan
}

data {
  int<lower = 0> I;
  int<lower = 0> J;
  int<lower = 0> indices[J];
  real X[I, J];
}

parameters {
  real beta[I];
}

model {
  print(size(X));
  for(i in 1:I) {
    for(j in indices) {
      X[i, j] ~ normal(beta[i], 1);
    }
  }
}
