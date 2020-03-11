library(tidyverse)
library(ISLR)
library(boot)
library(caret)

WageData<-Wage%>%
  select(age,wage)

gauss<-function(x,center,spread){
  return(1/(sqrt(2*pi)*spread)*exp(-(x-center)^2/(2*spread^2)))
  }

#####################
smallcv<-10000
cv_data<-c()
for(k in 1:20){
  myCenters<-seq(10,80,by=k)
  for(Spread in 1:20){

    WageData<-Wage%>%
      select(age,wage)

    for(j in 1:length(myCenters)){
        WageData[,j+2]<-sapply(WageData$age ,
                                myCenters[j],
                                Spread,
                              FUN = gauss)
        }

    model<-glm(wage~.-age,data=WageData)
    CV<-cv.glm(WageData,model,K=10)$delta[1]
    cv_data<-append(cv_data,CV)
    if (CV<smallcv){
      smallcv<-CV
      mincenter<-myCenters
      minspread<-Spread
    }
  }
}
#########################

WageData<-Wage%>%
  select(age,wage)

for(j in 1:length(mincenter)){
  WageData[,j+2]<-sapply(WageData$age ,
                         mincenter[j],
                         minspread,
                         FUN = gauss)
}

model<-glm(wage~.-age,data=WageData)

WageData<-WageData %>%
        mutate(predictions=predict.glm(model,newdata=WageData))


    ggplot(data=WageData,aes(x=age,y=wage))+
      geom_point(alpha=0.5)+
      geom_smooth(se=F)+
      geom_line(aes(y=predictions),color="red")
    