rm(list = ls())
# devtools::install_github("dahtah/imager")
# also had to install Quartz on my Mac: https://www.xquartz.org/
library(tidyverse)
library(magrittr)
library(imager)
shelby<-load.image("Shelby.jpg")
shelby_gray<-grayscale(shelby)
plot(shelby_gray)
#Shelby SVD

shelby_matrix<-as.matrix(shelby_gray)
shelby_svd<-svd(shelby_matrix)
ggplot() +
  geom_point(aes(x=1:2448,y=shelby_svd$d/sum(shelby_svd$d)))+
               scale_y_log10() +
               labs(title="Scree Plot")
p<-8
Approx_Shelby<-shelby_svd$u[,1:p]%*%diag(shelby_svd$d[1:p])%*%t(shelby_svd$v[,1:p])

plot(as.cimg(Approx_Shelby))
