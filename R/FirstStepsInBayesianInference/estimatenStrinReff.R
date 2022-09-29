library(rstan)
library(gdata)
library(bayesplot)

#Data prep, I am using our internal outOfHomeDuration. Furthermore, I am using "change of Incidence" as if it were equivalent to the effective R value
#This is just a first try to get the model running
dataset <- read_csv("ReffOutOfHomeDuration.csv")
x <- dataset$outOfHomeDuration
y <- dataset$changeOfIncidence
N <- length(dataset$outOfHomeDuration)

stan_data <- list(N=N, x=x, y=y)

fit1 <- stan(
  file = "stringencyREff.stan",  # Stan program
  data = stan_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 1,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
  )


plot(fit1)

posterior <- extract(fit1)

par(mfrow = c(1,1))
plot(y ~ x)
abline( mean(posterior$b), mean(posterior$a), col = 6, lw = 2)
abline( mean(posterior$b) + mean(posterior$sigma), mean(posterior$a), col = 5, lw=2, lt=2)
abline( mean(posterior$b) - mean(posterior$sigma), mean(posterior$a), col = 5, lw=2, lt = 2)


#traceplots
plot(posterior$a, type = "l")
plot(posterior$b, type = "l")
plot(posterior$sigma, type = "l")

#parameter summaries
par(mfrow = c(1,3))

plot(density(posterior$a), main = "A")
abline(v = lm_alpha, col = 4, lty = 2)

plot(density(posterior$b), main = "B")
abline(v = lm_beta, col = 4, lty = 2)

plot(density(posterior$sigma), main = "Sigma")
abline(v = lm_sigma, col = 4, lty = 2)
