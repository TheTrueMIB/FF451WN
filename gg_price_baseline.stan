//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=1> N;
  vector[N] y;
  real<lower=0> sigma;
  real mu0;
  real<lower=0> tau0;
}

parameters {
  real mu;
}

model {
  mu ~ normal(mu0, tau0);
  y ~ normal(mu, sigma);
}

generated quantities {
  real y_next;
  y_next = normal_rng(mu, sigma);
}

