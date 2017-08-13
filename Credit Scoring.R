rm(list=ls())
full = read.csv("german_credit.csv")


splitdf <- function(dataframe, seed=NULL) {
if (!is.null(seed)) set.seed(seed)
index <- 1:nrow(dataframe)
trainindex <- sample(index, trunc(3*length(index)/4))
trainset <- dataframe[trainindex, ]
testset <- dataframe[-trainindex, ]
list(trainset=trainset,testset=testset)
}


a = splitdf(full,seed = 999)
train = a$trainset
test = a$testset
rm(a)
#using simple rpart
library(rpart)
model_rpart = rpart(Creditability~.,data = train,method = "class")
library(rpart.plot)
plot(model_rpart)
rpart.plot(model_rpart)

preds = predict(model_rpart,newdata = test,type = "class")
a = table(test$Creditability,preds)
1 - ((a[2]+a[3])/(a[1]+a[2]+a[3]+a[4]))

#combining different rpart models 
library(foreach)
length_divisor<-3
iterations<-1000
predictions<-foreach(m=1:iterations,.combine=cbind) %do% 
  {
training_positions <- sample(nrow(train), size=floor((nrow(train)/length_divisor)))
train_pos<-1:nrow(train) %in% training_positions
model = rpart(Creditability~.,data = train[train_pos,],method = "class")
predict(model,newdata=test)[,2]
  }

predictions_rpart<-rowMeans(predictions)
predict= round(predictions_rpart)
a = table(test$Creditability,predict)
1 - ((a[2]+a[3])/(a[1]+a[2]+a[3]+a[4]))


#random forest
library(randomForest)

train$Creditability = as.factor(train$Creditability)
model = randomForest(Creditability~.,data = train,nodesize = 10)
predictions_rf = predict(model,newdata = test,type = "prob")[,2]
preds = round(preds[,2])
a = table(test$Creditability,preds)
1 - ((a[2]+a[3])/(a[1]+a[2]+a[3]+a[4]))


#combining rpart and random forest
model = randomForest(Creditability~.,data = train)

predictions_rf = predict(model,newdata = test,type = "prob")[,2]
predictsum = (predictions_rpart+predictions_rf)/2

predict = round(predictsum)
a = table(test$Creditability,predict)
1 - ((a[2]+a[3])/(a[1]+a[2]+a[3]+a[4]))

#using support vector machine
library(e1071)
model = svm(Creditability~.,data=train,probability = TRUE)
predisvm = predict(model,newdata = test,probability = TRUE)
predictions_svm = attr(predisvm, "probabilities")[,1]

a = table(test$Creditability,predisvm)
1 - ((a[2]+a[3])/(a[1]+a[2]+a[3]+a[4]))

#using gradient boosting method
library(gbm)
a = splitdf(full,seed=999)
train = a$trainset
test = a$testset


Model = gbm(Creditability~.,data = train

, distribution = "adaboost"

, n.trees = 2000

, shrinkage = 0.1

, interaction.depth = 4

, n.minobsinnode = 10

, verbose = FALSE
)
predictgbm = round(predict(Model,newdata = test,n.trees = 250,type = "response"))
a = table(test$Creditability,predictgbm)
1 - ((a[2]+a[3])/(a[1]+a[2]+a[3]+a[4]))

#combining rpart,random Forest,svm and gbm
predictions_gbm = predict(Model,newdata = test,n.trees = 250,type = "response")
predictsum = (predictions_rpart+predictions_rf+predictions_gbm+predictions_svm)/4

predict = round(predictsum)
a = table(test$Creditability,predict)
1 - ((a[2]+a[3])/(a[1]+a[2]+a[3]+a[4]))

#trying different ratio
predictsum = (2*predictions_rpart+6*predictions_rf+2*predictions_gbm)/10
t



#Super Learner is a Ensemble package
library(SuperLearner)
library(devtools)

#making knn,glm and randomforest ensembling
SL.library <- c("SL.knn",
"SL.glm",
"SL.randomForest")
method <- "method.NNLS"
family <- "binomial"

Y = train$Creditability
X = train[,-1]
newX = test[,-1]

fit <- SuperLearner(Y = Y, X = X,
family = family,
SL.library = SL.library,
method = method)


pred <- predict(fit, newdata = newX,X = X,Y = Y)
a = table(test$Creditability,round(pred$pred))
1 - ((a[2]+a[3])/(a[1]+a[2]+a[3]+a[4]))


#making knn,glm,randomforest,svm and gbm ensembling
SL.library <- c("SL.knn",
"SL.glm",
"SL.randomForest","SL.svm","SL.gbm")
method <- "method.NNLS"
family <- "binomial"
fit <- SuperLearner(Y = Y, X = X,
family = family,
SL.library = SL.library,
method = method)
pred <- predict(fit, newdata = newX,X = X,Y = Y)
a  = pred$pred
a = table(test$Creditability,round(a))
1 - ((a[2]+a[3])/(a[1]+a[2]+a[3]+a[4]))
