rm(list=ls())
concrete<-read.csv("Concrete_Data.csv")

A<-as.matrix(concrete[,1:8])
ATA<-t(A)%*%A
eig_ata<-eigen(ATA)
AAT<-A%*%t(A)
eign_aat<-eigen(AAT)
U<-eig_aat$vectors
V<-eig_ata$vectors 
Sigma<-matrix(0,nrow=1030,ncol=8)
for (j in 1:8){
  Sigma[j,j]<-sqrt(eig_ata$values[j])
}
plot(diag(Sigma))