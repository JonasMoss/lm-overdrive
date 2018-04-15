real [ , ] get_parameters(int N, int Q, int P, int[] no_unbounded, int[] no_positive,
             int[] no_unit, real[] beta_unbounded, real[] beta_positive,
             real[] beta_unit, int [ , ] unbounded_indices, int [ , ] positive_indices,
             int [ , ] unit_indices, real [ , ] X, int[] link_types) {

  real params[N, Q];

  int unbounded_index = 1;
  int positive_index = 1;
  int unit_index = 1;

  for(n in 1:N) {
    for(q in 1:Q) params[n, q] = 0;
  }

  for(q in 1:Q) {

    if(no_unbounded[q] > 0) {
      for(p in 1:P) {
        if(unbounded_indices[q, p] == 1) {
          for(n in 1:N) {
            params[n, q] = params[n, q] + X[n, p] * beta_unbounded[unbounded_index];
          }
          unbounded_index += 1;
        }
      }
    }

    if(no_positive[q] > 0) {
      for(p in 1:P) {
        if(positive_indices[q, p] == 1) {
          for(n in 1:N) {
            params[n, q] = params[n, q] + X[n, p] * beta_positive[positive_index];
          }
          positive_index += 1;
        }
      }
    }

    if(no_unit[q] > 0) {
      for(p in 1:P) {
        if(unit_indices[q, p] == 1) {
          for(n in 1:N) {
            params[n, q] = params[n, q] + X[n, p] * beta_unit[unit_index];
          }
          unit_index += 1;
        }
      }
    }
  }

  // Link handling

  for(q in 1:Q) {
    if(link_types[q] == 2) {
      for(n in 1:N) params[n, q] = 1/params[n, q];
    } else if (link_types[q] == 3) {
      for(n in 1:N) params[n, q] = 1/sqrt(params[n, q]);
    } else if (link_types[q] == 4) {
      for(n in 1:N) params[n, q] = exp(params[n, q]);
    } else if (link_types[q] == 5) {
      for(n in 1:N) params[n, q] = params[n, q]^2;
    }
  }

  return params;

}


