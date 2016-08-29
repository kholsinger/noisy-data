data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real mu_x;
  real mu_y;
  real<lower=0> sigma_x;
  real<lower=0> sigma_y;
}
transformed parameters {
  real mu_diff;
  mu_diff <- mu_x - mu_y;
}
model {
  mu_x ~ normal(0.0, 1.0);
  mu_y ~ normal(0.0, 1.0);
  sigma_x ~ normal(0.0, 1.0);
  sigma_y ~ normal(0.0, 1.0);
  x ~ normal(mu_x, sigma_x);
  y ~ normal(mu_y, sigma_y);
}