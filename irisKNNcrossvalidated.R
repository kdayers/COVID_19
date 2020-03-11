rm(list=ls())
library(tidyverse)
library(class)
library(caret)
library(boot)
# Goals: build a knn model to predict the Species in the Iris dataset.
# Cross validate to determine the optimal number of neighbors
# Display a plot of Flexibility vs cross validated error
# Make a decision on the optimal number of neighbors for this model.

# read in the data
iris <- iris

NumFolds <- 10
# set up the folds
folds <- createFolds(1:nrow(iris), k = NumFolds, returnTrain = TRUE)
# set up the number of neighbors you're going to check
MaxNeighbors <- 50
# set up a storage location for the future cross validation error estimates
cv_data <- data.frame(
  Neighbors = 1:MaxNeighbors, 
  CV = rep(0,MaxNeighbors)
)


for (K in 1:MaxNeighbors) { # loop over the number of neighbors
  # set up space for each CV estimate
  cv_class_error <- rep(0,NumFolds)
  
  for (j in 1:NumFolds) {  # loop for cross validation
    # training data for knn 
    train <- iris[folds[[j]], 1:4 ]
    train_class <- iris$Species[folds[[j]]]
    # testing data for knn
    test <- iris[-folds[[j]], 1:4 ]
    test_class <- iris$Species[-folds[[j]]]
    
    # build the knn model
    knn_model <- knn(
      train = train,
      test = test,
      cl = train_class,
      k = K
    )
    # store the value of the classification error for each fold
    cv_class_error[j] <- mean( knn_model != test_class  )
  } # end the loop
  
  # store the mean cv error for each choice of neighbors
  cv_data$CV[K] <- mean( cv_class_error  )
}

# plot
ggplot(data=cv_data, aes(x=1/Neighbors, y=CV)) + 
  geom_line() + geom_point()

# show the cross validation errors in order
head(cv_data %>% arrange(CV))