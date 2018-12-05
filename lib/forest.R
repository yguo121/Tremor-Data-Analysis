########################################
############# Random Forest ########
########################################
library(caret)

library(MASS)
library(randomForest)

load("../data/train_set/train_5.RData")
dt_train <- as.data.frame(dt_train)
dt_train$label <- as.factor(dt_train$label)

load("../data/test_set/test_5.RData")
dt_test <- as.data.frame(dt_test)
dt_test$label <- as.factor(dt_test$label)

# Normalize the Data
for (i in 1:18) {
  dt_train[,i] <- (dt_train[,i] - mean(dt_train[,i]))/sd(dt_train[,i])
  dt_test[,i] <- (dt_test[,i] - mean(dt_test[,i]))/sd(dt_test[,i])
}


## Train
forest_fit <- randomForest(label ~ ., data = dt_train, ntree=1000, mtry = 5)
#forest_fit <- randomForest(label ~ ampGravity_z + freqRotation_x + ampGravity_y + ampAcceleration_z + ampGravity_x + ampRotation_x + ampAcceleration_x + ampAcceleration_y + ampRotation_z + freqAcceleration_y, data = dt_train, ntree = 100, mtry = 5)


plot(forest_fit)
forest_fit

### Rank Variable Importance
varImpPlot(forest_fit,  
           sort = T,
           n.var=18,
           main="Top 18 - Variable Importance")


###Test

forest_pred <- predict(forest_fit,newdata = dt_test)
table(forest_pred,dt_test$label)
confusionMatrix(data = forest_pred, reference = dt_test$label)

