rm(list=ls())
concrete<-read.csv("Concrete_Data.csv")
X<-matrix(nrow=nrow(concrete),ncol=ncol(concrete))
X[,1]<-rep(1)
for(j in 1:8){
  X[,j+1]<-concrete[,j]
}

#X[,2]<-concrete$Cement




A<-t(X)%*%X
eigen(A)

Z<-X
Z[,2]<-(X[,2]-mean(X[,2]))/sd(X[,2])
#or scale(X[2])

B<-t(Z)%*%Z
eigen(B)