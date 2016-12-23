data {
  int N;           // Sample size
  int P;           // Feature size
  matrix[N, P] X;  // Data
  real alpha;      // L1 parameter
}
parameters {
  corr_matrix[P] Sigma; // Covariance matrix
}
model {
  vector[P] zeros;
  for (i in 1:P) {
     zeros[i] = 0;
  }
  // Precision matrix follows laplace distribution
  to_vector(inverse(Sigma)) ~ double_exponential(0, alpha);
  
  for (j in 1:N){
    // X follows multi normal distribution
    X[j] ~ multi_normal(zeros, Sigma);   
  }
}
generated quantities {
  matrix[P, P] Lambda;
  Lambda = inverse(Sigma);
}