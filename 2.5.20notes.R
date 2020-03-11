rm(list=ls())
library(tidyverse)
library(ISLR)
library(boot)
library(caret)
addata<-read.csv("Advertising.csv")%>%
  select(-X)

#Leave One Out Cross Validation
#LOOCV

LOOCV_mse<-c()
for (j in 1:200){
  df<-addata[-j,]
  model<-lm(sales ~ ., data=df)
  #dynamic memory allocation
  LOOCV_mse[j]<-(addata$sales[j]-predict.lm(model,addata[j,]))^2
  #only 1 thing in testing set
}

(LOOCV<-mean(LOOCV_mse))
#parens around prints it


LOOCV_mse<-c()
for (j in 1:200){
  df<-addata[-j,]
  model<-lm(sales ~ newspaper, data=df)
  #dynamic memory allocation
  LOOCV_mse[j]<-(addata$sales[j]-predict.lm(model,addata[j,]))^2
  #only 1 thing in testing set
}

(LOOCV<-mean(LOOCV_mse))



LOOCV_mse<-c()
for (j in 1:200){
  df<-addata[-j,]
  model<-lm(sales ~ newspaper+TV, data=df)
  #dynamic memory allocation
  LOOCV_mse[j]<-(addata$sales[j]-predict.lm(model,addata[j,]))^2
  #only 1 thing in testing set
}
(LOOCV<-mean(LOOCV_mse))


folds<-createFolds(1:nrow(addata),k=5,returnTrain=TRUE)
LOOCV_mse<-c()
for (j in 1:200){
  df<-addata[folds[[j]],]
  model<-lm(sales ~ ., data=df)
  #dynamic memory allocation
  LOOCV_mse[j] <- mean( (addata$sales[-folds[[j]]] - 
                           predict.lm(model, addata[-folds[[j]],]))^2)
}
(LOOCV<-mean(LOOCV_mse))

