library(tidyverse)
library(ISLR)
WageData<-Wage %>%
  select(age,wage)

ggplot(data=WageData,aes(x=age,y=wage))+
  geom_point(alpha=0.5,color="#CC0099")+
#  geom_smooth(color="purple",se=F)


#ggplot(data=WageData,aes(x=wage))+
#  geom_density(fill="pink")

  
  b1<-function(x){
    if (x>10 & x<=10){
      return(1)
    } else{
      return(0)
    }
    
  }

b2<-function(x){
  if (x>20 & x<=30){
    return(1)
  } else{
    return(0)
  }
  
}

b3<-function(x){
  if (x>30 & x<=40){
    return(1)
  } else{
    return(0)
  }
  
}

b4<-function(x){
  if (x>40 & x<=50){
    return(1)
  } else{
    return(0)
  }
  
}

b5<-function(x){
  if (x>50 & x<=60){
    return(1)
  } else{
    return(0)
  }
  
}

b6<-function(x){
  if (x>60 & x<=70){
    return(1)
  } else{
    return(0)
  }
  
}

b7<-function(x){
  if (x>70 & x<=80){
    return(1)
  } else{
    return(0)
  }
  
}

WageData<-WageData%>%
  mutate(
    basis1=sapply(age,b1),
    basis2=sapply(age,b2),
    basis3=sapply(age,b3),
    basis4=sapply(age,b4),
    basis5=sapply(age,b5),
    basis6=sapply(age,b6),
    basis7=sapply(age,b7)
  )

model<-glm(wage~.-age,data=WageData)

library(boot)
(cv_error<-cv.glm(WageData,model,K=10)$delta[1])

WageData<-WageData %>%
  mutate(
    predictions = predict.glm(model,newdata=WageData)
  )

ggplot(data=WageData,aes(x=age,y=wage))+
  geom_point()+
  geom_smooth(se=F)+
  geom_line(aes(y=predictions),color="red")
  
