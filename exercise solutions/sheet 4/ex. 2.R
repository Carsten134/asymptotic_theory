## requirements ############################################
library(tidyverse)

## config ##################################################
n <- c(30, 60, 100, 1000)
lambda <- 3
estimate_points <- -100:400/100
set.seed(123)

## exercise ################################################
eval_ecdf <- function(data, estimate_points) {
  result <- rep(NA, length(estimate_points))
  i <- 1
  for (x in estimate_points) {
    result[i] <- sum(data <= x)/length(data)
    i = i+1
  }
  return(result)
}

sim_ecdf <- function(n, lambda, estimate_points) {
  data <- rexp(n, lambda)
  estimates <- eval_ecdf(data, estimate_points)
  return(estimates)
}

sim_ecdf_sup <- function(n, lambda, estimate_points){
  estimates <- sim_ecdf(n, lambda, estimate_points)
  true_values <- pexp(estimate_points, 3)
  return(max (abs(estimates-true_values)))
}

# building the dataset for the four plots of the cdfs

plot_data <- data.frame(n = rep(NA, 2*length(estimate_points)*length(n)),
                        value = rep(NA, 2*length(estimate_points)*length(n)),
                        type = rep(NA, 2*length(estimate_points)*length(n)))

plot_data$value[1:(length(estimate_points)*length(n))] <- rep(pexp(estimate_points), length(n))
plot_data$type[1:(length(estimate_points)*length(n))] <- "true"

n_data <- rep(NA, length(estimate_points)*length(n))

for (i in 1:length) {
  l_b = (i-1)*(length(estimate_points))+1
  u_b = i*(length(estimate_points))
  n_data[l_b,u_p] <- n[i]
}

plot_data$n = rep(n_data, 2)


for(i in 1:length(n)) {
  l_b = (i-1)*(length(estimate_points))+1
  u_b = i*(length(estimate_points))
  plot_data$n[l_b:u_b] <- n[i]
  plot_data$estimate[l_b:u_b] <- sim_ecdf(n[i], lambda, estimate_points)
  plot_data$true_value[l_b:u_b] <- pexp(estimate_points, lambda)
}

plot_data %>% ggplot(aes(x = estimate_points))
