library(tseries)
library(ggplot2)
library(forecast)
library(caret)
library(tidyverse)
library(e1071)
library(car)
library(broom)
library(MASS)
load("C:/Users/Tim/Documents/GitHub/Tremor-Data-Analysis/lib/train_5.RData") 
load("C:/Users/Tim/Documents/GitHub/Tremor-Data-Analysis/lib/test_5.RData") 
head(dt_train)
dt_train <- as.data.frame(dt_train)
dt_test <- as.data.frame(dt_test)
# Assumptions
# https://www.statisticssolutions.com/assumptions-of-logistic-regression/

## First, binary logistic regression requires the dependent variable to be binary
factor(dt_train$label)
## The dependent variable is binary

## Second, There is a linear relationship between the logit of the outcome and each predictor variables.
xnam <- names(dt_train[-ncol(dt_train)])
formula <- as.formula(paste("label ~ ", paste(xnam, collapse= "+")))
model <- glm(formula, data = dt_train, 
             family = binomial)
probabilities <- predict(model, type = "response")
mydata <- dt_train %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

## Third, Influential points

plot(model, which = 4, id.n = 3)
model.data <- augment(model) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = label), alpha = .5) +
  theme_bw()
model.data %>% 
  filter(abs(.std.resid) > 3)

## Fourth, all variables are independent from each other

car::vif(model)

# which(vif(model)<10)
# new_dt_vif <- new_dt[,c(which(vif(model)!=max(vif(model))),ncol(new_dt))]
# dim(new_dt_vif)
# model1 <- glm(label ~ ., data=new_dt_vif, family="binomial")


###### Building model

xnam <- names(dt_train[-ncol(dt_train)])
formula <- as.formula(paste("label ~ ", paste(xnam, collapse= "+")))
model <- glm(formula, data = dt_train, 
             family = binomial)
model0 <- glm(label~1, data = dt_train, 
              family = binomial)
#### Higher order
formula1 <- as.formula(paste("label ~ ", paste(paste0(xnam,"+I(",xnam,"^2)"), collapse= "+")))

model1 <- glm(formula1, data = dt_train, 
             family = binomial)
model.aic <- stepAIC(model1, direction = "backward",
        trace = FALSE, scope = list(upper = model1, lower = model))

###### Test for goodness of fit
### Likelihood Ratio
anova(model1,model,model.aic,test="Chisq")

### Hoslem
library(ResourceSelection)
hoslem.test(dt_train$label, fitted(model), g=20)
hoslem.test(dt_train$label, fitted(model1), g=20)
hoslem.test(dt_train$label, fitted(model.aic), g=20)

### Confusion Matrix
fitted.probs0 = predict(model, type='response')
preds0 <- ifelse(fitted.probs0 > 0.5, 1, 0)
fitted.probs1 = predict(model1, type='response')
preds1 <- ifelse(fitted.probs1 > 0.5, 1, 0)
fitted.probs.aic = predict(model.aic, type='response')
preds.aic <- ifelse(fitted.probs.aic > 0.5, 1, 0)

accuracy0 <- table(preds0, dt_train$label)
accuracy0
accuracy1 <- table(preds1, dt_train$label)
accuracy1
accuracy.aic <- table(preds.aic, dt_train$label)
accuracy.aic

sum(diag(accuracy0))/sum(accuracy0)
sum(diag(accuracy1))/sum(accuracy1)
sum(diag(accuracy.aic))/sum(accuracy.aic)

### ROC Curve
library(pROC)
roc0 <- roc(dt_train$label~fitted.probs0)
roc1 <- roc(dt_train$label~fitted.probs1)
roc.aic <- roc(dt_train$label~fitted.probs.aic)
plot(roc0)
plot(roc1)
plot(roc.aic)
roc0
roc1
roc.aic

#### Model Testing
set.seed(1000)
mod_fit <- train(formula1,  data=dt_train, method="glm", family="binomial")

prob <- predict(mod_fit, newdata=dt_test)
pred <- ifelse(prob > 0.5, 1, 0)
pred <- factor(pred)
accuracy <- table(pred, factor(dt_test$label))
accuracy
sum(diag(accuracy))/sum(accuracy)

confusionMatrix(data=pred, factor(dt_test$label))
