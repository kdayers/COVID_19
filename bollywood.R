library(tidyverse)

bollywood<-read.csv("Bollywood.csv")

ggplot(data=bollywood,aes(x=Budget,y=Gross))+
  geom_point(color="purple",shape=3)

