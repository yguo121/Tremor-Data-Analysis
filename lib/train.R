########################################
############# Modeling ########
########################################

train <- function(train_set, label_set, params=NULL, run_rf = F){
  
  rf <- NULL
  if(run_rf){
    if( !require("randomForest" )){
      install.packages("randomForest")
    }
    
    library(randomForest)
    library(caret)
    library(e1071)
    
    rf.fit <- randomForest(as.factor(label_set) ~ .,
                           data = featMat, mtry = params[1],
                           importance=TRUE, 
                           ntree = params[2])
    modelList[[i]] <- list(fit= rf.fit)
  }
}


