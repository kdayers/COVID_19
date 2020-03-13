rm(list=ls())
library(tidyverse)
library(boot)
library(caret)
library(class)
library(e1071)

wine<-read.csv("winequality-red.csv")%>%
  mutate(
    quality = as.factor(ifelse(quality<=5,"Low","High")))


svm_model<-svm(quality~.,data=wine,kernel="linear",cost=12,scale=TRUE)

#compare predicted with actual - this is a 2x2 matrix
confusion<-table(predictions=svm_model$fitted,actua=wine$quality)
class_err<-1-sum(diag(confusion))/sum(confusion)


#cross-validate

tuned_svm<-tune.svm(quality~.,data=wine,kernel="linear",cost=c(0.1,1,10,100))

tuned_svm$best.model

tuned_svm$performances%>%
  arrange(error)