rm(list=ls())
library(tidyverse)
library(boot)
library(caret)

X<-as.matrix(iris[,-5])

for (j in 1:4){
  X[,j]<-X[,j]-mean(X[,j])
}

svdX<-svd(X)

#(explained_variance<-svdX$d/sum(svdX$d))

#%cumsum(explained_variance)

#ggplot()+
#  geom_col(aes(x=1:(ncol(iris)-1),y=explained_variance),fill="purple",color="red")

p=2
 U<-svdX$u[,1:p]          
iris<-iris
numneigh<-50
kcosts<-data.frame(k=seq(1,numneigh),
                   meancost=rep(0,numneigh))


for (neigh in 1:numneigh){
  folds<-createFolds(1:nrow(iris),k=10,returnTrain=TRUE)
  costs<-rep(0,10)
  for (j in 1:10){
    irisfold<-iris[folds[[j]],]
    knn_model<-knn(
      train=U[folds[[j]],],
      test=U[-folds[[j]],],
      cl=irisfold$Species,
      k=neigh
    )
    testing<-iris[-folds[[j]],]%>%
      mutate(SpeciesTest=knn_model)%>%
      mutate(Correct=ifelse(SpeciesTest==Species,0,1))
    
    #    for (i in 1:nrow(testing)){
    #     if (testing$SpeciesTest[i]==Testing$Species[i]){
    #      testing$Correct[i]<-0
    #    }
    #   else {
    #    testing$Correct[i]<-1
    #  }
    
    
    costs[j]<-1/nrow(testing)*sum(testing$Correct)
  }
  kcosts$meancost[neigh]<-mean(costs)
}

kcosts<-kcosts%>%
  mutate(krecip=1/k)

ggplot(data=kcosts,aes(x=k,y=meancost))+
  geom_line()+geom_point()+
  scale_x_reverse()


kcostsarr<-kcosts%>%
  arrange(meancost)
