rm(list=ls())

library(tidyverse)
library(pracma)

A<-matrix(0,10,5)
for (i in 1:10){
    A[i,]<-runif(5,min=-1,max=1)
}

aat<-A%*%t(A)
ata<-t(A)%*%A

ataeig<-eigen(ata)
aateig<-eigen(aat)

ataeig$values
aateig$values

atavec<-ataeig$vectors
aatvec<-aateig$vectors
dotsata<-matrix(0,5,5)
for (i in 1:5){
  for (j in 1:5){
    dotsata[i,j]<-dot(atavec[,i],atavec[,j])
  }
}
norm(dotsata-diag(5),"f")

dotsaat<-matrix(0,10,10)
for (i in 1:10){
  for (j in 1:10){
    dotsaat[i,j]<-dot(aatvec[,i],aatvec[,j])
  }
}

norm(dotsaat-diag(10),"f")

U<-aatvec[,1:5]
V<-atavec
Sigma<-diag(sqrt(ataeig$values))
#Sigma<-matrix(0,10,5)
#Sigma[1:5,]<-diag(sqrt(ataeig$values))
norm(A-U%*%Sigma%*%t(V),"2")




