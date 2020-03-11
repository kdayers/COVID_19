library(tidyverse)
concrete<-read.csv("Concrete_Data.csv")
y<-concrete$Concrete_compressive_strength
X<-matrix(rep(1,2*nrow(concrete)),nrow=nrow(concrete),byrow=TRUE)
X[ ,2]<-concrete$Cement
A<-t(X)%*%X #matrix multiplication, with transpose
b<-t(X)%*%y
beta<-solve(A,b)
model<-lm(Concrete_compressive_strength~Cement,data=concrete)
summary(model)