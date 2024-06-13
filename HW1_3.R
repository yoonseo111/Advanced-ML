library(mvtnorm)
library(ggplot2)

mu1 <- c(7, 7)
mu2 <- c(3, 5)
sigma <- matrix(c(2, 0, 0, 2), nrow = 2, ncol = 2)
num_samples <- 100

# (a) Generate 100 observations for each class
set.seed(123)
X1 <- rmvnorm(num_samples, mu1, sigma)
X2 <- rmvnorm(num_samples, mu2, sigma)

# Combine data
data <- rbind(cbind(X1, Y = 1), cbind(X2, Y = -1))
colnames(data) <- c("X1", "X2", "Y")
data <- as.data.frame(data)

# (b) Plot the observations
plot(data$X1, data$X2, col = ifelse(data$Y == 1, "blue", "red"), xlab = "X1", ylab = "X2")

# (c) Draw the Bayesian decision boundary
x <- seq(0, 10, length.out = 100)
y <- seq(0, 10, length.out = 100)
grid <- expand.grid(X1 = x, X2 = y)
Z <- matrix(NA, nrow = length(x), ncol = length(y))

for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    Z[i, j] <- (x[i] - mu1[1])^2 / sigma[1, 1] + (y[j] - mu1[2])^2 / sigma[2, 2] - 
      (x[i] - mu2[1])^2 / sigma[1, 1] - (y[j] - mu2[2])^2 / sigma[2, 2]
  }
}

contour(x, y, Z, levels = 0, col = "black", add = TRUE)