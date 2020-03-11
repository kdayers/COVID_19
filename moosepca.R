rm(list = ls())
devtools::install_github("dahtah/imager")
# also had to install Quartz on my Mac: https://www.xquartz.org/
library(tidyverse)
library(imager)
moose<-load.image("moose.jpeg")
moose_gray<-grayscale(moose)
plot(moose_gray)

#Moose SVD

moose_matrix<-as.matrix(moose_gray)
moose_svd<-svd(moose_matrix)

ggplot() +
  geom_point(aes(x=1:1275,y=moose_svd$d/sum(moose_svd$d)),size=0.25,color="blue")+
  scale_y_log10()+
  labs(title="Scree Plot")

p<-8
Approx_Moose<-moose_svd$u[,1:p]%*%diag(moose_svd$d[1:p])%*%t(moose_svd$v[,1:p])

plot(as.cimg(Approx_Moose))