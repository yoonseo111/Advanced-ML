library(ISLR)
library(caret)

data("Auto")
set.seed(123)

# (a) Create 5 folds of data
folds <- createFolds(Auto$mpg, k = 5)

# (a) Check the dimension of each fold
for (i in 1:5) {
  cat("Fold", i, ":", length(folds[[i]]), "observations\n")
}

# (b) Set up matrix to store cross-validation errors
cv_errors <- matrix(NA, nrow = 5, ncol = 10)

# Nested for-loops to calculate cross-validation errors
for (fold in 1:5) {
  for (degree in 1:10) {
    # Get training and validation data for the current fold
    train_data <- Auto[-folds[[fold]], ]
    val_data <- Auto[folds[[fold]], ]
    
    # Fit polynomial regression model
    model <- lm(mpg ~ poly(horsepower, degree, raw = TRUE), data = train_data)
    
    # Predict on validation data
    predictions <- predict(model, newdata = val_data)
    
    # Calculate mean squared error
    cv_errors[fold, degree] <- mean((predictions - val_data$mpg)^2)
  }
}

# Visualize the errors
matplot(1:10, t(cv_errors), type = "l", lty = 1, col = 1:5, xlab = "Degree", ylab = "Mean Squared Error",
        main = "Cross-Validation Errors for Polynomial Regression")
legend("topright", legend = paste("Fold", 1:5), col = 1:5, lty = 1)

# (c) Compute mean cross-validation error for each degree
mean_cv_errors <- apply(cv_errors, 2, mean)

# Visualize the errors
plot(1:10, mean_cv_errors, type = "l", col = "blue", xlab = "Degree", ylab = "Mean Squared Error",
     main = "Mean Cross-Validation Errors for Polynomial Regression")

# (d)
optimal_degree <- which.min(mean_cv_errors)
print(optimal_degree)

