## config ######################################################################
n = c(10,30,100,1000)
lambda = 3
m = 1000

# seed
set.seed(123)

## simulation ##################################################################

# using the result from exercise 1 we get, that S_n has variance n*lambda
# also X_ni ~ poi(lambda) => E(X_ni) = lambda, Var(X_ni) = lambda

sim_zn <- function(n, lambda) {
  X_n <- rpois(n, lambda = lambda)
  S_n <- sum((X_n-lambda))
  Z_n <- S_n/(sqrt(n*lambda))
  return(Z_n)
}

sim_zn_sample <- function(m, n, lambda) {
  data <- replicate(m, sim_zn(n, lambda))
  return(data)
}

# a)

sim_data <- data.frame("10" = sim_zn_sample(m, 10, lambda),
                       "30" = sim_zn_sample(m, 30, lambda),
                       "100" = sim_zn_sample(m, 100, lambda),
                       "1000" = sim_zn_sample(m, 1000, lambda))


hist(sim_data$X10,
     xlim = c(-5,5),
     main = "n = 10",
     xlab = "Z_n")

hist(sim_data$X30,
     xlim = c(-5,5),
     main = "n = 30",
     xlab = "Z_n")

hist(sim_data$X100,
     xlim = c(-5,5),
     main = "n = 100",
     xlab = "Z_n")

hist(sim_data$X1000,
     xlim = c(-5,5),
     main = "n = 1000",
     xlab = "Z_n")

# convergence in distribution to standard normal
# Lyapunov-condition does hold for delta = 1


# b)

sim_data <- data.frame("10" = sim_zn_sample(m, 10, lambda/10),
                       "30" = sim_zn_sample(m, 30, lambda/30),
                       "100" = sim_zn_sample(m, 100, lambda/100),
                       "1000" = sim_zn_sample(m, 1000, lambda/1000))


hist(sim_data$X10,
     xlim = c(-3,3),
     main = "n = 10",
     xlab = "Z_n")

hist(sim_data$X30,
     xlim = c(-3,3),
     main = "n = 30",
     xlab = "Z_n")

hist(sim_data$X100,
     xlim = c(-3,3),
     main = "n = 100",
     xlab = "Z_n")

hist(sim_data$X1000,
     xlim = c(-5,5),
     main = "n = 1000",
     xlab = "Z_n")


# no convergence in distribution to normal
# Lyapunov-condition does not hold for delta = 1

# c)
sim_zn <- function(n, lambda) {
  X_n <- rpois(n, lambda = (lambda/(n-1)))
  X_n[1] <- rpois(1, (n-1)*lambda)
  S_n <- sum((X_n-lambda))
  # using independence we can compute the variance by summing over all variances
  # of X_ni 1<= i <= n
  # (n-1)*lambda + ((n-1)/(n-1))*lambda = (n-1+1)*lambda = n*lambda
  Z_n <- S_n/(sqrt(n*lambda))
  return(Z_n)
}

sim_data <- data.frame("10" = sim_zn_sample(m, 10, lambda),
                       "30" = sim_zn_sample(m, 30, lambda),
                       "100" = sim_zn_sample(m, 100, lambda),
                       "1000" = sim_zn_sample(m, 1000, lambda))

hist(sim_data$X10,
     xlim = c(-5,5),
     main = "n = 10",
     xlab = "Z_n")

hist(sim_data$X30,
     xlim = c(-5,5),
     main = "n = 30",
     xlab = "Z_n")

hist(sim_data$X100,
     xlim = c(-5,5),
     main = "n = 100",
     xlab = "Z_n")

hist(sim_data$X1000,
     xlim = c(-5,5),
     main = "n = 1000",
     xlab = "Z_n")


# convergence in distribution to standard normal
# Lyapunov under delta = 1 holds (look at notes)