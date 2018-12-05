library(tseries)
library(ggplot2)
library(forecast)
library(caret)
library(e1071)
load("C:/Users/Tim/Documents/GitHub/Tremor-Data-Analysis/lib/train.RData") 
load("C:/Users/Tim/Documents/GitHub/Tremor-Data-Analysis/lib/test.RData") 
head(dt_train)
dt_train <- as.data.frame(dt_train)
dt_train$label <- factor(dt_train$label)
dt_test <- as.data.frame(dt_test)
# Assumptions
# https://www.statisticssolutions.com/assumptions-of-logistic-regression/

## First, binary logistic regression requires the dependent variable to be binary
factor(dt_train$label)
## The dependent variable is binary

## Second, logistic regression requires the observations to be independent of each other.
## In other words, the observations should not come from repeated measurements or matched data.

#### Can use adf test to test for autocorrelation
adf_test <- c(adf.test(dt_train[,1])$p.value)
for(i in 2:(length(dt_train)-1)){
  adf_test <- rbind(adf_test,adf.test(dt_train[,i])$p.value)
}
adf_test

## Autocorrelation exists. Test for independency fails. Skip to line 38. 
## Find the set of data that are linearly independent.

# Use arima to extract indepent observations
# arima <- c()
# for(i in 1:(length(dt_train)-1)){
#   print(auto.arima(dt_train[,i]))
# }
# 
# ## The least common multiplier is a multiple of 3*2=6.
indep <- c(0,0,0)
for(j in 1:20){
  for(k in 1:j){
    index <- seq(k,k+as.integer(nrow(dt_train)/(j))*(j),(j))
    new_dt <- dt_train[index,]
    adf_test_new <- c(adf.test(new_dt[,1][!is.na(new_dt[,1])])$p.value)
    for(i in 2:(length(dt_train)-1)){
      adf_test_new <- rbind(adf_test_new,adf.test(new_dt[,i][!is.na(new_dt[,1])])$p.value)
    }
    indep <- rbind(indep,c(k,j,length(which(adf_test_new>0.05))))
  }
}
colnames(indep) <- c("Starting Index",
                     "Length", 
                     "Independent Parameters")
indep[indep[,3]==18,]
## To create independent observation, it is best to start from 13 observation 
## and take one observation from every 15 observations

index <- seq(13,13+as.integer(nrow(dt_train)/(15))*(15),(15))
new_dt <- dt_train[index,]
new_dt <- new_dt[complete.cases(new_dt),]
adf_test_new <- c(adf.test(new_dt[,1][!is.na(new_dt[,1])])$p.value)
for(i in 2:(length(dt_train)-1)){
  adf_test_new <- rbind(adf_test_new,adf.test(new_dt[,i][!is.na(new_dt[,1])])$p.value)
}
length(which(adf_test_new>0.05))
## The ADF test checked out for new_dt
dim(new_dt)
## Only 41 observations left. Could cause overfitting


## Third, logistic regression requires there to be little or no multicollinearity among the independent variables.  
## This means that the independent variables should not be too highly correlated with each other.

#### Use vif to find the correlated variables.

model <- glm(label ~ ., data=new_dt, family="binomial")
vif(model)
which(vif(model)<10)
new_dt_vif <- new_dt[,c(which(vif(model)<10),ncol(new_dt))]
model1 <- glm(label ~ ., data=new_dt_vif, family="binomial")

## Fourth, logistic regression assumes linearity of independent variables and log odds.
log_odds <- log(exp(model1$fitted.values)/(1+exp(model1$fitted.values)))
plot(new_dt_vif[,5],log_odds)
#### Unfinished


###### Building model
###### https://www.r-bloggers.com/evaluating-logistic-regression-models/
mod_fit <- train(label ~ .,  data=new_dt_vif, method="glm", family="binomial")
summary(mod_fit)

###### Test for goodness of fit
library(ResourceSelection)
hoslem.test(new_dt_vif$label, fitted(mod_fit), g=10)

###### Very small p-value. Overfitting

pred = predict(mod_fit, newdata=dt_test)
accuracy <- table(pred, dt_test[,"label"])
sum(diag(accuracy))/sum(accuracy)

confusionMatrix(data=pred, factor(dt_test$label))

#### Comparison with uncleaned data
mod_fit1 <- train(label ~ .,  data=new_dt, method="glm", family="binomial")
mod_fit2 <- train(label ~ .,  data=dt_train, method="glm", family="binomial")

pred1 = predict(mod_fit1, newdata=dt_test)
pred2 = predict(mod_fit2, newdata=dt_test)

confusionMatrix(data=pred1, factor(dt_test$label))
confusionMatrix(data=pred2, factor(dt_test$label))
