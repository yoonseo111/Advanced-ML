## linear regression, polynomial regression, spline

library(ggplot2)
library(splines)

# data generation
set.seed(123)
x = seq(0, 1, length.out = 100)
f = function(x){
  return(sin(2*pi*x^3))
}
y = f(x) + rnorm(100, 0, 0.1) 
# rnom(n, mean, sd) -> add random noise

data = data.frame(x = x, y = y)

# Univariate Regression
lm_fit = lm(y ~ x, data = data)

# Polynomial Regression
poly_fit = lm(y ~ poly(x, degree = 3), data = data)

# Cubic (natural) Spline Regression
spline_fit = lm(y ~ ns(x, df = 8), data = data)

# visualization
ggplot(data = data, aes(x = x, y = y)) +
  geom_point() + # point
  # add smoothed lines
  # se -> confidence interval
  geom_smooth(method = "lm", formula = y~x, se = FALSE,
              aes(color = "Univariate"), linetype = "dashed") +
  geom_smooth(method = "lm", formula = y~poly(x, degree=3),
              se = FALSE, aes(color = "Polynomial")) +
  geom_smooth(method = "lm", formula = y~ns(x, df=8),
              se = FALSE, aes(color = "Spline")) +
  labs(title = "Regression Methods Comparison",
       x="X",
       y="Y",
       color = "Method") +
  theme_minimal() # background setting

print(summary(lm_fit))
print(summary(poly_fit))
print(summary(spline_fit))


## kernel ridge regression

l2norm = function(x){
  return(sum(x^2))
}

gaussian_kernel = function(x1, x2, rho){
  return(exp(-rho * l2norm(x1 - x2)))
}

gaussian_kernel_mat = function(x,rho){
  if (class(x)[1] == "numeric")
    x = matrix(x)
  n = nrow(x)
  # Initialize -> Start with an empty matrix
  K = matrix(0, nrow=n, ncol=n)
  for (i in  1:n){
    for (j in 1:n){
      K[i, j] = gaussian_kernel(x[i, ], x[j, ], rho)
    }
  }
  return(K)
}

rho = 10
lamda = 0.001
K = gaussian_kernel_mat(x, rho)
# t(y) -> transpose y
# %*% -> matrix multiplication
y_hat = t(y) %*% solve(K + lamda * diag(100)) %*% K

data_pl = data
data_pl$y_hat = as.numeric(y_hat)
ggplot(data = data_pl, aes(x=x, y=y)) +
  geom_point() +
  geom_line(aes(x=x, y=f(x), color = "True"), linewidth = 1.2) +
  geom_line(aes(x=x, y=y_hat, color = "KRR"), linewidth = 1.2) +
  theme_minimal() +
  labs(title = "Kernel Ridge Regression",
       x = "X",
       y = "Y",
       color = "")

## Concrete data

rm(list = ls())
library(AppliedPredictiveModeling)
library(caret)

data("concrete")
# concrete와 mixtures 중 mixtures data 사용
str(mixtures)

featurePlot(x = mixtures[, -9],
            y = mixtures$CompressiveStrength,
            # space between plots
            between = list(x=1, y=1),
            type = c("g", "p", "smooth"))

library(corrplot)

# 종속변수 제외한 나머지 변수들 간의 상관관계
correlation = cor(mixtures[, -9])
corrplot(correlation, type="upper", addCoef.col = "black",
         tl.col = "black")

library(plyr)
averaged = ddply(mixtures,
                 .(Cement, BlastFurnaceSlag, FlyAsh, Water,
                   Superplasticizer, CoarseAggregate, FineAggregate, Age),
                 function(x) c(CompressiveStrength =
                                 mean(x$CompressiveStrength)))

library(caret)
set.seed(185)

# p -> proportion of the training set
# list = FALSE -> output is a vector
train_rows = createDataPartition(averaged$CompressiveStrength,
                                 p=0.7, list = FALSE)
head(train_rows)

# split the data
train_data = averaged[train_rows, ]
test_data = averaged[-train_rows,]

# linear regression
library(caret)

lm_m = lm(CompressiveStrength ~ ., data = train_data)
summary(lm_m)

pred_lm = predict(lm_m, test_data)
head(pred_lm)

lm_comp = data.frame(obs = test_data[, 9],
                     pred = pred_lm)
# Calculate performance
defaultSummary(lm_comp)

# penalized linear regression
library(caret)
library(glmnet)
# glmnet implements elastic net with mixing parameter alpha
# alpha = 1 leads to the lasso and alpha = 0 leads to the ridge

grid_lasso_lamda = expand.grid(.alpha = 1,
                               .lambda = seq(0.01, 0.2, length.out = 30))
set.seed(11)
lm_lasso_fit = train(train_data[, 1:8], train_data[, 9],
                     method = "glmnet",
                     metric = "RMSE",
                     preProc = c("center", "scale"),
                     tuneGrid = grid_lasso_lamda,
                     trControl = trainControl(method = "cv",
                                              number = 5))
lm_lasso_fit

pred_lasso = predict(lm_lasso_fit, test_data[, 1:8])

lasso_comp = data.frame(obs = test_data[, 9],
                        pred = pred_lasso)
defaultSummary(lasso_comp)

## ridge

grid_ridge_lambda = expand.grid(.alpha = 0,
                                .lambda = seq(0.01, 0.2, length.out=30))
set.seed(11)
lm_ridge_fit = train(train_data[, 1:8], train_data[, 9],
                     method = "glmnet",
                     metric = "RMSE",
                     preProc = c("center", "scale"),
                     tuneGrid = grid_ridge_lambda,
                     trControl = trainControl(method="cv",
                                              number = 5))
lm_ridge_fit

pred_ridge = predict(lm_ridge_fit, test_data[, 1:8])

ridge_comp = data.frame(obs = test_data[,9],
                        pred = pred_ridge)
defaultSummary(ridge_comp)

## additive model and partial linear model
library(mgcv)

# generalized additive model. s() represents a smooth term
gam_m = gam(CompressiveStrength ~ s(Cement) + s(BlastFurnaceSlag)
            + s(FlyAsh) + s(Water) + s(Superplasticizer)
            + s(CoarseAggregate) + s(FineAggregate) + s(Age),
            data = train_data)
summary(gam_m)

# visualize the relationship between x and y
par(mfrow = c(2,4))
for (i in 1:8)
  plot(gam_m, select = i, lwd = 2, residuals = TRUE)

pred_gam = predict(gam_m, test_data[, 1:8])
gam_comp = data.frame(obs = test_data[, 9],
                      pred = pred_gam)
defaultSummary(gam_comp)

## partially linear model
# based on the above residual plots, the effects of Cement,
# CoarseAggregate, FineAggreate are close to linear

# s() -> nonlinear prediction -> flexible
plm_m = gam(CompressiveStrength ~ Cement + s(BlastFurnaceSlag)
            + s(FlyAsh) + s(Water) + s(Superplasticizer)
            + CoarseAggregate + FineAggregate + s(Age),
            data =train_data)

summary(plm_m)

par(mfrow = c(2,4))
for (i in 1:5)
  plot(plm_m, select = i, lwd = 2, residuals = TRUE)
termplot(plm_m, se=TRUE, col.term=1, col.se=1, rug=TRUE)

pred_plm = predict(plm_m, test_data[,1:8])
plm_comp = data.frame(obs=test_data[,9], pred = pred_plm)
defaultSummary(plm_comp)
