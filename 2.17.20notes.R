library(tidyverse)
library(class)
library(RColorBrewer)
library(caret)


iris<-iris

ggplot(data=iris,aes(x=Sepal.Length,
                     y=Sepal.Width,
                     color=Species,
                     shape=Species))+
  geom_point(size=3)+
  scale_color_brewer(palette="Paired")


testing<-expand.grid(
  Sepal.Length = seq(4,8, by = 0.05),
  Sepal.Width = seq(1.8,4.5,by = 0.05)
)

knn_model<-knn(
  train=iris%>%
    select(Sepal.Length,Sepal.Width),
  test=testing,
  cl=iris$Species,
  k=5
)
testing<-testing%>%
  mutate(
    Species=knn_model
  )

ggplot() + 
  geom_point(data=testing, 
             aes(x=Sepal.Length,
                 y=Sepal.Width,
                 color=Species), 
             alpha = 0.25) + 
  geom_point(data=iris, 
             aes(x=Sepal.Length,
                 y=Sepal.Width,
                 color=Species,
                 shape=Species),
             size=2)
