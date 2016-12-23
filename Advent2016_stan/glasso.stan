data {
  int N;           // Sample size
  int P;           // Feature size
  matrix[N, P] X;  // Data
  real alpha;      // scale parameter of double exponential (L1 parameter)
}
parameters {
  corr_matrix[P] Lambda; // Covariance matrix
}
model {
  vector[P] zeros;
  for (i in 1:P) {
     zeros[i] = 0;
  }

  // Precision matrix follows laplace distribution
  to_vector(Lambda) ~ double_exponential(0, 1/alpha);

  for (j in 1:N){
    // X follows multi normal distribution
    X[j] ~ multi_normal(zeros, inverse(Lambda));
  }
}
generated quantities {
  matrix[P, P] Sigma;
  Sigma = inverse(Lambda);
}
