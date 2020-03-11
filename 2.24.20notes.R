rm(list=ls())
library(tidyverse)
library(GGally)

iris<-iris
iris$Species<-as.factor(iris$Species)

iris_predictors<-iris%>%
  select(-Species)

iris_svd<-svd(iris_predictors)

ggpairs(iris,aes(color=Species,alpha=0.3))

pca_data<-data.frame(iris_svd$u,Species=iris$Species)

ggpairs(pca_data,aes(color=Species,alpha=0.3))

spheres<-c(2,pi)
cubes<-c(2,4)
ratios<-c(1,pi/4)

for (n in 3:40){
  spheres<-c(spheres,2*pi*spheres[n-2]/n)
  cubes<-c(cubes,2^n)
  ratios<-c(ratios,spheres[n]/cubes[n])
}

ggplot()+
  geom_point(aes(x=1:40,y=ratios))
