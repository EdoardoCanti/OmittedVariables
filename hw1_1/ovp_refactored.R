library(MASS)

# Sigma for scenario 1
SCENARIO1_SIGMA <- matrix(c(1,0.5,0.5,1), 2,2);
# Sigma for scenario 2
SCENARIO2_SIGMA <- matrix(c(1,0,0,1),2,2);
# Mean for scenarios
MU_ZEROES <- matrix(c(0,0),2,1)
# Number of samples
n_samples <- 100
# coeffiecents for the linear model y = beta0 + beta1*X1 + beta2*X2 + epsilon
beta0 <- 1
beta1 <- 1
beta2 <- -2
# number of simulation
sim_num <- 1000
# Setting the seed for reproducibility
set.seed(99)

# Data Generating Process function
dgp <- function(n, sigma, beta_0, beta_1, beta_2){
  Z <- mvrnorm(n = n, MU_ZEROES, sigma)
  epsilon <-rnorm(n = n_samples, 0, 1)
  X1 <- Z[,1]
  X2 <- Z[,2]
  y <- beta_0 + beta_1*X1 + beta_2*X2 + epsilon
  return(list(X1 = X1, X2 = X2, y = y))
}

# n_samples = 100 / sim_num = 1000 
betas_scenario1 <- replicate(sim_num, {
  # Data Generating Process
  dgp_res <- dgp(n=n_samples, sigma=SCENARIO1_SIGMA, beta_0=beta0, beta_1=beta1, 
                 beta_2=beta2)
  X1 <- dgp_res$X1
  X2 <- dgp_res$X2
  y <- dgp_res$y
  # Estimating params of a regression linear model
  #   model with only X1
  m0 <- lm(y ~ X1)
  current_beta1_m0 <- summary(m0)$coeff[2,1]
  #   model with both X1 and X2
  m1 <- lm(y ~ X1 + X2)
  current_beta1_m1 <- summary(m1)$coeff[2,1]
  c(current_beta1_m0, current_beta1_m1)
})

# Plotting betas 1 for both models in scenario 1
par(mfrow=c(1,1))
plot(betas_scenario1[1,], pch=16, col="#0D7680", main="Scenario1: beta1 (incomplete model)", xlab = "Simulation num", ylab="beta1")
plot(betas_scenario1[2,], pch=16, col="#DD4C2D", main="Scenario1: beta1 (complete model)", xlab = "Simulation num", ylab="beta1")

# Plotting histograms of betas 1 for both models in scenario 1
par(mfrow=c(1,2))  
hist(betas_scenario1[1,], col="#0D7680", prob=TRUE, main="Beta1 (model X1)", xlab="Beta1")
hist(betas_scenario1[2,], col="#DD4C2D", prob=TRUE, main="Beta1 (model X1+X2)", xlab="Beta1")

betas_scenario2 <- replicate(sim_num, {
  # Data Generating Process
  dgp_res <- dgp(n=n_samples, sigma=SCENARIO2_SIGMA, beta_0=beta0, beta_1=beta1, beta_2=beta2)
  X1 <- dgp_res$X1
  X2 <- dgp_res$X2
  y <- dgp_res$y
  # Estimating params of a regression linear model
  #   model with only X1
  m0 <- lm(y ~ X1)
  current_beta1_m0 <- summary(m0)$coeff[2,1]
  #   model with both X1 and X2
  m1 <- lm(y ~ X1 + X2)
  current_beta1_m1 <- summary(m1)$coeff[2,1]
  c(current_beta1_m0, current_beta1_m1)
})

# Plotting betas 1 for both models in scenario 2
par(mfrow=c(1,1))
plot(betas_scenario2[1,], pch=16, col="#0D7680", main="Scenario2: beta1 (incomplete model)", xlab = "Simulation num", ylab="beta1")
plot(betas_scenario2[2,], pch=16, col="#DD4C2D", main="Scenario2: beta1 (complete model)", xlab = "Simulation num", ylab="beta1")

# Plotting histograms of betas 1 for both models in scenario 2
par(mfrow=c(1,2))  
hist(betas_scenario2[1,], col="#0D7680", prob=TRUE, main="Beta1 (model X1)", xlab="Beta1")
hist(betas_scenario2[2,], col="#DD4C2D", prob=TRUE, main="Beta1 (model X1+X2)", xlab="Beta1")


