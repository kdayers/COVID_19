library(MASS)
library(tidyverse)

model<-lm(medv~.,data=Boston)
summary(model)
X<-matrix(nrow=nrow(Boston),ncol=ncol(Boston))

y<-Boston$medv
X[,1]<-rep(1)
for(j in 1:13){
  X[,j+1]<-Boston[,j]
}
A<-t(X)%*%X
b<-t(X)%*%y
beta<-solve(A,b)
evalA<-eigen(A)
condA<-max(evalA$values)/min(evalA$values)

Z<-X
for(j in 1:13){
  Z[,j+1]<-scale(Boston[,j])
}
B<-t(Z)%*%Z
evalB<-eigen(B)
condB<-max(evalB$values)/min(evalB$values)