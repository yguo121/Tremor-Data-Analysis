########################################
############# SVM ########
########################################

library(e1071)
library(caret)

load("../data/train_set/train.RData")
dt_train <- as.data.frame(dt_train)
dt_train$label <- as.factor(dt_train$label)

load("../data/test_set/test.RData")
dt_test <- as.data.frame(dt_test)
dt_test$label <- as.factor(dt_test$label)

# Normalize the Data
for (i in 1:18) {
  dt_train[,i] <- (dt_train[,i] - mean(dt_train[,i]))/sd(dt_train[,i])
  dt_test[,i] <- (dt_test[,i] - mean(dt_test[,i]))/sd(dt_test[,i])
}

# Train svm
svm_fit <- svm(dt_train$label ~ ., data = dt_train, kernel = "radial", cost = 5)

# Test
svm_pred <- predict(svm_fit,newdata = dt_test)
confusionMatrix(data = svm_pred, reference = dt_test$label)
