library(tidyverse,caret)
library(boot)
df<-read.csv("GreatCrossValidationChallenge.csv")%>%
  select(-X)

ggplot(data=df, aes(x=x,y=y))+
  geom_point()

############
haar<-function(x,mu,s){
  if ((mu-s)<=x && x<mu){
    return(1)
  }else if(mu<=x && x<(mu+s)){
    return(-1)
  }
  else{
    return(0)
  }
}
#############

smallcv<-10000
cv_data<-c()
for(k in seq(5,50,by=5)){
  mus<-seq(0,200,by=k)
  for(s in 1:20){
    
    df<-df%>%
      select(x,y)
    
    for(j in 1:length(mus)){
      df[,j+2]<-sapply(df$x ,
                             mus[j],
                             s,
                             FUN = haar)
    }
    
    model<-glm(y~.-x,data=df)
    CV<-cv.glm(df,model,K=10)$delta[1]
    cv_data<-append(cv_data,CV)
    if (CV<smallcv){
      smallcv<-CV
      minmu<-mus
      mins<-s
    }
  }
}
df<-df%>%
  select(x,y)


model<-glm(y~.-x,data=df)
for(j in 1:length(minmu)){
  df[,j+2]<-sapply(df$x ,
                   minmu[j],
                   mins,
                   FUN = haar)
}

df<-df %>%
  mutate(predictions=predict.glm(model,newdata=df))


ggplot(data=df,aes(x=x,y=y))+
  geom_point(alpha=0.5)+
  geom_smooth(se=F)+
  geom_line(aes(y=predictions),color="red")

