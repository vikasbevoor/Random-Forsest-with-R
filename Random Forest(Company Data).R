install.packages("randomForest")
library(randomForest)

library(C50)
install.packages("caret")
library(caret)

install.packages("moments")
library(moments)

View(Comp)
attach(Comp)

# Data exploration
summary(Comp)
str(Comp)

# Graphical exploaration
hist(Sales)
summary(Sales)
skewness(Sales)
kurtosis(Sales)
qqnorm(Sales)
qqline(Sales)

hist(CompPrice)
summary(CompPrice)
skewness(CompPrice)
kurtosis(CompPrice)
qqnorm(CompPrice)
qqline(CompPrice)

hist(Income)
summary(Income)
skewness(Income)
kurtosis(Income)
qqnorm(Income)
qqline(Income)

hist(Price)
summary(Price)
skewness(Price)
kurtosis(Price)
qqnorm(Price)
qqline(Price)

# Splitting the sales into two categories 
sale <- ifelse(Sales <= 10,"Low","High")
View(sale)

Comp1 <- Comp[-1]
Comp1 <- cbind(Comp1,sale)
View(Comp1)

# Data partion for model building and testing
inTraininglocal <- createDataPartition(Comp1$sale,p=.75, list=F)
training <- Comp1[inTraininglocal,]
testing <- Comp1[-inTraininglocal,]


# Building a random forest model on training data (500 trees) 
fit.forest <- randomForest(sale~.,data=training, na.action=na.roughfix,importance=TRUE, ntree=500)

# Predicting test data 
pred_test <- predict(fit.forest,newdata=testing)
mean(pred_test==testing$sale) 

# Confusion Matrix 
confusionMatrix(testing$sale, pred_test)

# Building a random forest model on training data (1000 trees)
fit.forest <- randomForest(sale~.,data=training, na.action=na.roughfix,importance=TRUE, ntree=1000)

# Predicting test data 
pred_test <- predict(fit.forest,newdata=testing)
mean(pred_test==testing$sale) 

# Confusion Matrix 
confusionMatrix(testing$sale, pred_test)

# Building a random forest model on training data (200 trees) 
fit.forest1 <- randomForest(sale~.,data=training, na.action=na.roughfix,importance=TRUE, ntree=200)

# Predicting test data 
pred_test <- predict(fit.forest1,newdata=testing)
mean(pred_test==testing$sale) 

# Confusion Matrix 
confusionMatrix(testing$sale, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

importance(fit.forest)
varImpPlot(fit.forest)

#Boosting technique
install.packages("xgboost")
require(xgboost)
require(methods)
library(xgboost)
View(Comp1)

cmp <- Comp1[-c(6,9,10)]
View(cmp)

datagbm<-cmp
str(datagbm)
library(caret)

mean<-c()
for(i in 1:400){
  
  inTraining <- createDataPartition(datagbm$sale, p = .75, list = FALSE)
  training <- datagbm[ inTraining,]
  testing  <- datagbm[-inTraining,]
  
  labeltraining = as.numeric(training[[8]])
  datatraining = as.matrix(training[1:7])
  
  
  labeltesting  = as.numeric(testing [[8]])
  datatesting  = as.matrix(testing [1:7])
  
  xgtraining <- xgb.DMatrix(data=datatraining, label = labeltraining)
  
  xgtesting <- xgb.DMatrix(data=datatesting, label = labeltesting)
  
  param = list("objective" = "multi:softmax", "bst:eta" = 0.005,"bst:max_depth" = 4,"num_class"=4,"nthread" = 4,"gamma" =0.5,"min_child_weight" = 3)
 
  model = xgboost(params = param, data = xgtraining, nround = 100)#,subsample = 0.8,colsample_bytree = 0.8)
 
  ypred = predict(model, xgtesting)
  
  mean <- c(mean,mean(labeltesting==ypred) )
  
}

summary(mean)