## Dataset
# Breast Cancer data

rm(list = ls())
library(tidyverse)

url = "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
data = read.csv(file = url, header = FALSE,
                col.names = c("ID","clump_thickness", "uniformity_size", "uniformity_shape", "marginal_adhesion", "single_epithelial_cell_size", "bare_nuclei", "bland_chromatin", "normal_nucleoli","mitoses", "diagnosis"))

str(data)

# count "?" in bare_nuclei
sum(data$bare_nuclei == "?")

# excluding the id and rows with missing
# pipe function %>% -> 이저 단계의 결과를 다음 함수의 입력으로 전달
select = dplyr::select
data = select(data, -1)
data = data[data$bare_nuclei != "?",] %>% dplyr::mutate(bare_nuclei = as.integer(as.character((bare_nuclei))))

# 0 for bengin(양성) and 1 for malignant(악성)
data = data %>% dplyr::mutate(diagnosis = ifelse(diagnosis==2, 0, 1),
                              diagnosis = as.factor(diagnosis))
summary(data)

ggplot(data, aes(x=diagnosis, fill=diagnosis)) +
  geom_bar() +
  ggtitle("Distribution of Diagnosis in the Entire Dataset") +
  theme_minimal()

library(corrplot)

plot.new(); dev.off()
correlation = cor(data[,-10])
corrplot(correlation, type="upper", addCoef.col = "black", tl.col="black")

# Data splitting
library(caret)
set.seed(185)

train_rows = createDataPartition(data$diagnosis, p=0.6,
                                 list=FALSE)
head(train_rows)

# split the data
train_data = data[train_rows, ]
test_data = data[-train_rows, ]

## logistic regression
logistic_m = glm(diagnosis ~ .,
                 data = train_data,
                 family = binomial)
summary(logistic_m)

pred_prob_logit = predict(logistic_m, test_data, type="response")

pred_class_logit = ifelse(pred_prob_logit > 0.5, 1, 0)

# accuracy
mean(pred_class_logit == test_data[, 10])

confusionMatrix(data = factor(pred_class_logit),
                reference = test_data[, 10])$table

## linear and quadratic discriminant analysis
library(MASS)

pre_process_procedure = preProcess(train_data[, 1:9])

train_data_pre = train_data
test_data_pre = test_data
train_data_pre[, 1:9] = predict(pre_process_procedure,
                                newdata = train_data[, 1:9])
test_data_pre[, 1:9] = predict(pre_process_procedure,
                               newdata = test_data[, 1:9])

# LDA
lda_m = lda(x = train_data_pre[, 1:9],
            grouping = train_data_pre[, 10])

# discriminant vector
lda_m$scaling

lda_test = predict(lda_m, test_data_pre[, 1:9])

# accuracy
mean(lda_test$class == test_data[,10])

# reference -> real data
confusionMatrix(data = lda_test$class,
                reference = test_data[, 10])$table

# QDA
qda_m = qda(x = train_data_pre[, 1:9],
            grouping = train_data_pre[, 10])

qda_test = predict(qda_m, test_data_pre[, 1:9])

# accuracy
mean(qda_test$class == test_data[,10])

confusionMatrix(data = qda_test$class,
                reference = test_data[, 10])$table

# logistic regression with lasso penalty
alpha_value = 1
grid_lambda = expand.grid(.alpha = alpha_value,
                          .lambda = seq(0.01, 0.2, length.out=30))
set.seed(11)
logit_lasso_fit = train(train_data[, 1:9], train_data[, 10],
                        method = "glmnet",
                        metric = "Accuracy",
                        family = "binomial",
                        preProc = c("center", "scale"),
                        tuneGrid = grid_lambda,
                        trControl = trainControl(method="cv",
                                                 number = 5))
logit_lasso_fit

# prediction with optimal parameter lamda = 0.02965517
pred_class_logit_lasso = predict(logit_lasso_fit,
                                 test_data[, 1:9])

mean(pred_class_logit_lasso == test_data[, 10])

confusionMatrix(data = pred_class_logit_lasso,
                reference = test_data[, 10])$table

logit_lasso_m = glmnet(train_data[, 1:9], train_data[, 10],
                       standardize = TRUE,
                       alpha = 1,
                       lambda = 0.02965517,
                       family = "binomial")
logit_lasso_m$beta

## naive Bayes
library(klaR)

nbayes_m = NaiveBayes(diagnosis ~ .,
                      data = train_data,
                      usekernel = TRUE,
                      fL=2)
pred_bayes = predict(nbayes_m, test_data)
head(pred_bayes$posterior)

pred_class_bayes = pred_bayes$class

mean(pred_class_bayes == test_data[, 10])

confusionMatrix(data = pred_class_bayes,
                reference = test_data[, 10])$table

## K-nearest neighbor
library(class)

pre_process_procedure = preProcess(train_data[, 1:9])

train_data_pre = train_data
test_data_pre = test_data
train_data_pre[, 1:9] = predict(pre_process_procedure,
                                newdata = train_data[, 1:9])
test_data_pre[, 1:9] = predict(pre_process_procedure,
                               newdata = test_data[, 1:9])

knn_m = knn3(x = train_data_pre[, 1:9],
             y = train_data_pre[, 10],
             k = 3)

# proportion
head(predict(knn_m, test_data_pre[, 1:9]))

pred_class_knn = predict(knn_m, test_data_pre[, 1:9],
                         type="class")
mean(pred_class_knn == test_data[, 10])

# tuning k
set.seed(123)
knn_fit = train(train_data[, 1:9], train_data[, 10],
                method = "knn",
                metric = "Accuracy",
                preProc = c("center", "scale"),
                tuneGrid = data.frame(.k = 1:15),
                trControl = trainControl(method="cv", number=5))
knn_fit

# prediction with optimal parameter k=7
knn_m = knn3(x = train_data_pre[, 1:9],
             y = train_data_pre[, 10],
             k=7)

pred_class_knn = predict(knn_m, test_data_pre[, 1:9],
                         type="class")
mean(pred_class_knn == test_data[, 10]) 

confusionMatrix(data = pred_class_knn,
                reference = test_data[, 10])$table

## support vector machine

library(kernlab)

sigma_value = 1 / ncol(train_data[, -10])
grid_sigma_C = expand.grid(.sigma = sigma_value,
                           .C = 2^(seq(-8, 2)))
set.seed(123)
svm_fit = train(train_data[, 1:9], train_data[, 10],
                method = "svmRadial",
                metric = "Accuracy",
                preProc = c("center", "scale"),
                tuneGrid = grid_sigma_C,
                # 5-fold cross validation
                trControl = trainControl(method = "cv",
                                         number = 5))
svm_fit

pred_class_svm = predict(svm_fit, test_data[, 1:9])

mean(pred_class_svm == test_data[, 10])
