library(tidyverse)
train <- read.csv("train.csv")
df <- train 

MSE <- function(df, beta){
  pred <- beta[1]
  for(p in 1:(ncol(df)-1)){
    pred <- pred + beta[p+1]*df[,p]
  }
  obs <- df[,ncol(df)]
  return( (1/nrow(df))*sum(obs-pred)^2 )
}
GradRSS <- function(df, beta, lambda){
  pred <- beta[1]
  for(p in 1:(ncol(df)-1)){
    pred <- pred + beta[p+1]*df[,p]
  }
  obs <- df[,ncol(df)]
  n <- nrow(df)
  g <- c((-2/n)*sum(obs-pred))
  for(p in 1:(ncol(df)-1)){
    g <- c(g, (-2/n)*sum( (obs-pred) * df[,p]) + 2*lambda*beta[p+1])
  }
  return(g)
}

beta <- runif(ncol(df),min=-100,max=100) # starting value
cost_values <- c()
n <- 1
for(n in 1:10000){
  ind<-sample(1:nrow(df), size=2000)
  sample_df<-df[ind, ]
  alpha <- 2/(n+1) # decaying learning rate
  beta <- beta - alpha * GradRSS(sample_df,beta,alpha) / norm(as.matrix(GradRSS(sample_df,beta,alpha)))
  cost_values[n] <- MSE(df,beta) 
}
print(beta)

ggplot() + 
  geom_line(aes(x=1:10000, y=cost_values), color="blue", size=0.5) + 
  scale_y_log10() + 
  labs(x="Iteration Number", y="Log of MSE", 
       title="MSE vs Iteration Number")