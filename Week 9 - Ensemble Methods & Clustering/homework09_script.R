### BUSINESS ANALYTICS ###
### Homework Sheet 9


## Homework 9.4

# We want to automatize the Expectation Maximization algorithm using R.
# This will allow us to run a higher number of phases and to solve different instances.
# However, we assume that we will stick to two clusters only.

# a) Write a function in R which, for a given vector x and parameters mu, sigma, returns the solution of f(x, mu, sigma)
myf <- function(x, mu, sigma) {
  firstFac <- (1 / (sigma*sqrt(2 * pi)))
  secondFac <- exp((-1) * ((x - mu)^2) / (2 * sigma^2))
  return(firstFac * secondFac)
}

# b) Initialize your start values.
values <- c(.76, .86, 1.12, 3.05, 3.51, 3.75)

mu_a <- 1.12
sigma_a <- 1
p_a <- .5

mu_b <- 3.05
sigma_b <- 1
p_b <- .5

# c) Build a for-loop which repeats the expectation and the maximization step for two times.
em_algo <- function(values, mu_a, sigma_a, p_a, mu_b, sigma_b, p_b, niter) {
  for(i in 1:niter) {
    # calculate likelihoods
    p_x_a <- sapply(values, myf, mu = mu_a, sigma = sigma_a)
    p_x_b <- sapply(values, myf, mu = mu_b, sigma = sigma_b)
    
    p_x <- (p_x_a * p_a) + (p_x_b * p_b)
    p_a_x <- (p_x_a * p_a) / p_x
    p_b_x <- (p_x_b * p_b) / p_x
    
    # update parameters
    mu_a <- sum(p_a_x * values) / sum(p_a_x)
    sigma_a <- sqrt(sum(p_a_x * ((values - mu_a)^2)) / (sum(p_a_x)))
    p_a <- sum(p_a_x) / sum(p_a_x + p_b_x)
    
    mu_b <- sum(p_b_x * values) / sum(p_b_x)
    sigma_b <- sqrt(sum(p_b_x * ((values - mu_b)^2)) / (sum(p_b_x)))
    p_b <- sum(p_b_x) / sum(p_a_x + p_b_x)
  }
  return(list(p_a_x, p_b_x))
}

em_algo(values, mu_a, sigma_a, p_a, mu_b, sigma_b, p_b, 2)

# d) Experiment with the following starting parameters and higher numbers of repetition (increase the counter in the for-loop).
#    What do you observe?

#    sigma_a = sigma_b = 1
#    p_a = p_b = 0.5

#    mu_a = .76 | .86
#    mu_b = 3.75 | 1.12

em_algo(values, .76, 1, .5, 3.75, 1, .5, 10)
em_algo(values, .86, 1, .5, 1.12, 1, .5, 10)

em_algo(values, .76, 1, .5, 3.75, 1, .5, 50)
em_algo(values, .86, 1, .5, 1.12, 1, .5, 50)

em_algo(values, .76, 1, .5, 3.75, 1, .5, 100)
em_algo(values, .86, 1, .5, 1.12, 1, .5, 100)

# choosing reasonable start values is crucial. When choosing these properly (i.e..76, 3.75), the algorithm converges to a clear solution quite quickly.
# However, if our start values are "badly" chosen (i.e. .86, 1.12) it might happen that there is no convergence after 10 iterations.
# Increasing the number of iterations, the algorithm might converge to a clear solution eventually. 
# Here, the first three data points (almost surely) belong to cluster A and the last three data points (almost surely) belong to cluster B.