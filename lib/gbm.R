########################################
############# GBM ################
########################################

library(mlbench)
library(gbm)

load("../data/train_set/train_5.RData")
dt_train <- as.data.frame(dt_train)
dt_train$label <- as.numeric(dt_train$label) - 1

load("../data/test_set/test_5.RData")
dt_test <- as.data.frame(dt_test)
dt_test$label <- as.factor(dt_test$label)

# Normalize the Data
for (i in 1:18) {
  dt_train[,i] <- (dt_train[,i] - mean(dt_train[,i]))/sd(dt_train[,i])
  dt_test[,i] <- (dt_test[,i] - mean(dt_test[,i]))/sd(dt_test[,i])
}

#For the classification, we use the bernoulli distribution. As the author suggested, normally, we should choose small shrinkage,such between 0.01 and 0.001; the number of trees, n.trees, is between 3000 and 10000
gbm_fit <- gbm(label~., data=dt_train, shrinkage=0.01, distribution = 'bernoulli', cv.folds = 5, n.trees=10000, interaction.depth = 4,verbose=F)

#Check the best iteration number.
best.iter = gbm.perf(gbm_fit, method="cv")

#Summary of the model results, with the importance plot of predictors.
summary(gbm_fit)

#Plots the marginal effect of the selected variables by "integrating" out the other variables.
plot.gbm(gbm_fit, 1, best.iter)

#Using the caret package the get the model preformance in the best iteration.
library(caret)

dt_train$label <- as.factor(dt_train$label) 

set.seed(123)
fitControl = trainControl(method="cv", number=5, returnResamp = "all")

model2 = train(label~., data=dt_train, method="gbm",distribution="bernoulli", trControl=fitControl, verbose=F, tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))
model2
confusionMatrix(model2)

gbm_predict <- predict(model2, dt_test, na.action = na.pass)
confusionMatrix(gbm_predict,factor(dt_test$label))
