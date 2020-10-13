View(Fraud)
library(C50)
install.packages("caret")
library(caret)

Fraud$Taxable.Income <- ifelse(Fraud$Taxable.Income <= 30000,"Risky","Good")
Fraud$Taxable.Income <- as.factor(Fraud$Taxable.Income)
View(Fraud)
str(Fraud)

# Data partion for model building and testing
inTraininglocal <- createDataPartition(Fraud$Taxable.Income,p=.75, list=F)
training <- Fraud[inTraininglocal,]
testing <- Fraud[-inTraininglocal,]

# Building a random forest model on training data (500 trees)
fit.forest <- randomForest(Taxable.Income~.,data=training, na.action=na.roughfix,importance=TRUE, ntree=500)

# Predicting test data 
pred_test <- predict(fit.forest,newdata=testing)
mean(pred_test==testing$Taxable.Income) 

# Confusion Matrix 
confusionMatrix(testing$Taxable.Income, pred_test)

# Building a random forest model on training data (1000 trees)
fit.forest <- randomForest(Taxable.Income~.,data=training, na.action=na.roughfix,importance=TRUE, ntree=1000)

# Predicting test data 
pred_test <- predict(fit.forest,newdata=testing)
mean(pred_test==testing$Taxable.Income) 

# Confusion Matrix 
confusionMatrix(testing$Taxable.Income, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

# Building a random forest model on training data (400 trees)
fit.forest <- randomForest(Taxable.Income~.,data=training, na.action=na.roughfix,importance=TRUE, ntree=400)

# Predicting test data 
pred_test <- predict(fit.forest,newdata=testing)
mean(pred_test==testing$Taxable.Income) 

# Confusion Matrix 
confusionMatrix(testing$Taxable.Income, pred_test)

importance(fit.forest)
varImpPlot(fit.forest)


#Boosting technique
require(xgboost)
require(methods)
library(xgboost)
View(Fraud)

datagbm<-Fraud
library(caret)

mean<-c()
for(i in 1:400){
  
  inTraining <- createDataPartition(datagbm$Taxable.Income, p = .75, list = FALSE)
  training <- datagbm[ inTraining,]
  testing  <- datagbm[-inTraining,]
  
  labeltraining = as.numeric(training[[3]])
  datatraining = as.matrix(training[4:5])
  
  labeltesting  = as.numeric(testing [[3]])
  datatesting  = as.matrix(testing [4:5])
  
  xgtraining <- xgb.DMatrix(data=datatraining, label = labeltraining)
  
  xgtesting <- xgb.DMatrix(data=datatesting, label = labeltesting)
  
  param = list("objective" = "multi:softmax", "bst:eta" = 0.005,"bst:max_depth" = 4,"num_class"=4,"nthread" = 4,"gamma" =0.5,"min_child_weight" = 3)
  
  model = xgboost(params = param, data = xgtraining, nround = 1000)#,subsample = 0.8,colsample_bytree = 0.8)
  
  ypred = predict(model, xgtesting)
  
  mean <- c(mean,mean(labeltesting==ypred) )
  mean
}

summary(mean)

