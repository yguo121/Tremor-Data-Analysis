########################################
############# Random Forest ########
########################################
library(caret)

## Train
library(MASS)
library(randomForest)
load("../data/train_set/train.RData")
dt_train <- as.data.frame(dt_train)
dt_train$label <- as.factor(dt_train$label)
hand_rf <- randomForest(label ~ ., data = dt_train, ntree=100, mtry = 5)
#hand_rf <- randomForest(label ~ ampGravity_z + freqRotation_x + ampGravity_y + ampAcceleration_z + ampGravity_x + ampRotation_x + ampAcceleration_x + ampAcceleration_y + ampRotation_z + freqAcceleration_y, data = dt_train, ntree = 100, mtry = 5)


plot(hand_rf)
hand_rf

### Rank Variable Importance
varImpPlot(hand_rf,  
           sort = T,
           n.var=18,
           main="Top 18 - Variable Importance")


###Test
dt_test <- as.data.frame(dt_test)
dt_test$label <- as.factor(dt_test$label)
hand_pred <- predict(hand_rf,newdata = dt_test)
table(hand_pred,dt_test$label)
confusionMatrix(data = hand_pred, reference = dt_test$label)

