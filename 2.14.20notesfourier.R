rm(list=ls())
library(tidyverse,caret)
library(boot)
df<-read.csv("GreatCrossValidationChallenge.csv")%>%
  select(-X)

ggplot(data=df, aes(x=x,y=y))+
  geom_point()

############
fourier<-function(x,omega){
  return(sin(2*pi*omega*x))
}
#############

smallcv<-10000
cv_data<-c()

for(k in seq(0.005,.05,by=.001)){
  omegas<-seq(0.001,.1,by=k)
    df<-df%>%
      select(x,y)
    
    for(j in 1:length(omegas)){
      df[,j+2]<-sapply(df$x ,
                       omegas[j],
                       FUN = fourier)
    }
    
    model<-glm(y~.-x,data=df)
    CV<-cv.glm(df,model,K=10)$delta[1]
    cv_data<-append(cv_data,CV)
    if (CV<smallcv){
      smallcv<-CV
      minomegas<-omegas

    }
}

############## 

df<-df%>%
  select(x,y)



for(j in 1:length(minomegas)){
  df[,j+2]<-sapply(df$x ,
                   minomegas[j],
                   FUN = fourier)
}

model<-glm(y~.-x,data=df)
CV<-cv.glm(df,model,K=10)$delta[1]

df<-df %>%
  mutate(predictions=predict.glm(model,newdata=df))


ggplot(data=df,aes(x=x,y=y))+
  geom_point()+
  geom_smooth(se=F)+
  geom_point(aes(y=predictions),size=0.01,color="red")

df<-df %>%
  mutate(SE=(df$y-df$predictions)^2)