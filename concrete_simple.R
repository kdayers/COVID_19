library(tidyverse)
concrete <- read.csv("Concrete_Data.csv")
df <- concrete %>% 
  select(Cement , Concrete_compressive_strength)
target_model <- lm(Concrete_compressive_strength ~ . ,data=df)
( target_coefficients <- target_model$coefficients )

MSE <- function(df, beta){
  pred <- beta[1] + beta[2]*df[,1]
  obs <- df[,2]
  return( (1/nrow(df))*sum(obs-pred)^2 )
}
GradRSS <- function(df, beta){
  pred <- beta[1] + beta[2]*df[,1]
  obs <- df[,2]
  n <- nrow(df)
  g1 <- (-2/n)*sum(obs-pred)
  g2 <- (-2/n)*sum( (obs-pred) * df[,1])
  return(c(g1,g2) )
}

beta <- c(13,0.1) # starting value
tol <- 1e-8
cost_values <- c()
n <- 1
for(n in 1:1200){
  alpha = 2/(n+1) # decaying learning rate
  beta <- beta - alpha * GradRSS(df,beta) / norm(as.matrix(GradRSS(df,beta)))
  cost_values[n] <- MSE(df,beta) 
}
print(beta)