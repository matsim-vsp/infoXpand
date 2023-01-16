data {
 int < lower = 1 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Outcome
}

parameters {
 real < lower = 0 > b; // Intercept
 real a; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
 a ~ normal(0, 0.1);
 b ~ lognormal(0, 1);
 sigma ~ cauchy(0,2);
 y ~ normal(b + x * a , sigma);
}

generated quantities {
} // The posterior predictive distribution"
