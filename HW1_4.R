library(splines)

set.seed(123)

# Define the regression model
regression_function <- function(x) {
  3*sin(2*pi*x^3) + 2*cos(2*pi*x)
}

# (a) Generate 200 data points
n <- 200
x <- runif(n, min = 0, max = 1)
epsilon <- rnorm(n, mean = 0, sd = sqrt(0.12))
y <- regression_function(x) + epsilon

# (a) Split the data
train_indices <- sample(1:n, 150)
x_train <- x[train_indices]
y_train <- y[train_indices]
x_val <- x[-train_indices]
y_val <- y[-train_indices]

# (a) Find the optimal degree
degrees <- 1:15
validation_errors_poly <- numeric(length(degrees))
for (deg in degrees) {
  model <- lm(y_train ~ poly(x_train, degree = deg, raw = TRUE))
  y_pred <- predict(model, newdata = data.frame(x = x_val))
  validation_errors_poly[deg] <- mean((y_pred - y_val)^2)
}

optimal_degree_poly <- which.min(validation_errors_poly)
print(optimal_degree_poly)

# (b) Determine the optimal degrees of freedom for the cubic spline regression
df <- 11:25
validation_errors_spline <- numeric(length(df))
for (dof in df) {
  model <- lm(y_train ~ ns(x_train, df = dof))
  y_pred <- predict(model, newdata = data.frame(x = x_val))
  validation_errors_spline[dof-10] <- mean((y_pred - y_val)^2)
}

optimal_dof_spline <- df[which.min(validation_errors_spline)]
print(optimal_dof_spline)

# (c) Generate 100 equally spaced points
x_test <- seq(0, 1, length.out = 150)

# Fit optimal polynomial regression
poly_model <- lm(y_train ~ poly(x_train, degree = optimal_degree_poly, raw = TRUE))
y_pred_poly <- predict(poly_model, newdata = data.frame(x = x_test))

# Fit optimal spline regression
spline_model <- lm(y_train ~ ns(x_train, df = optimal_dof_spline))
y_pred_spline <- predict(spline_model, newdata = data.frame(x = x_test))

# Calculate the range for ylim
y_range <- range(c(y_pred_poly, y_pred_spline, y_train))

# Visualize the result
plot(x_test, y_pred_poly, col = "red", lwd = 2, type = "l", lty = 1, ylim = y_range, 
     xlab = "x", ylab = "y", main = "Comparison of Polynomial and Spline Regression")
lines(x_test, y_pred_spline, col = "green", lwd = 2, type = "l", lty = 2)
points(x_train, y_train, col = "blue", pch = 16)
legend("topright", legend = c("Polynomial Regression", "Spline Regression", "Data"), 
       col = c("red", "green", "blue"), lty = c(1, 2, NA), lwd = c(2, 2, NA), bg = "white")
