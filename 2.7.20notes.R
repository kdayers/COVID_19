#rm(list=ls())
library(tidyverse,GGally,boot)
library(caret)


ads<-read.csv("Advertising.csv")

ads<-ads %>%
  select(-X)

#ggpairs(ads)

model<-glm(sales~.,data=ads)
billy<-cv.glm(ads,model,K=10)
cv<-billy$delta[1]

cv_data<-data.frame(TV=rep(0,64),
                    Radio=rep(0,64),
                    Newspaper=rep(0,64),
                    CV=rep(0,64))


j<-1
for(TVDeg in 1:4){
  for(RadioDeg in 1:4){
    for(NewspaperDeg in 1:4){
      model<-glm(sales~poly(TV,degree=TVDeg)+
                   poly(radio,degree=RadioDeg)+
                   poly(newspaper,degree=NewspaperDeg),
                  data=ads
                   )
    cv_data$TV[j]<-TVDeg
    cv_data$Radio[j]<-RadioDeg
    cv_data$Newspaper[j]<-NewspaperDeg
    billy<-cv.glm(ads,model,K=10)
    cv_data$CV[j]<-billy$delta[1]
    j=j+1
    
        }
  }
}
cv_data<-cv_data%>%
  arrange(
    CV
  )
cv_data

final_model<-glm(sales~poly(TV,degree=4)+
                   poly(radio,degree=1)+
                   poly(newspaper,degree=1),
                 data=ads
                 )

ads<-ads %>%
  mutate(
    Predictions = predict.glm(final_model, newdata=ads)
  )

ggplot(data=ads,aes(x=TV,y=sales))+
  geom_point()+
  geom_point(aes(y=Predictions),shape=3,color="red")

ggplot(data=ads,aes(x=radio,y=sales))+
  geom_point()+
  geom_point(aes(y=Predictions),shape=3,color="red")

ggplot(data=ads,aes(x=newspaper,y=sales))+
  geom_point()+
  geom_point(aes(y=Predictions),shape=3,color="red")
