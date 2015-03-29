require("ggplot2")
require("dplyr")

## Read input files 
setwd("./cancer_csv")
all_X <- read.csv("X.csv", header=FALSE)
all_Y <- read.csv("y.csv", header=FALSE)
setwd("../")
source("functions.R")

## Set up data structures 
test   <- c(1:183)
Xtest  <- all_X[test,]
Xtrain <- all_X[-test,]
Ytest  <- as.vector( all_Y[test,] )
Ytrain <- as.vector( all_Y[-test,] )

## Part 1
## Generate a discrete random variable distributed according to the CDF of W; 
## Plot its histogram for n=100, 200, 300, 400
W <- c(0.1, 0.2, 0.3, 0.4)
for (n in c(100, 200, 300, 400)) {
        c <- sampleDiscreteRV(n, W)
        ggplot(as.data.frame(c), aes(x=c)) +
                geom_histogram(binwidth=1, color="black", fill="white") +
                scale_x_discrete()
        fn <- paste0("hist",n,".png")
        ggsave(filename=fn)
}

## Part 2
## Implement a boosted linear Bayes classifier with sampling;
## Gaussian parameters are calculated from "boostrap" samples B_t
T <- 1000 

## Run on the training set
n <- length(Ytrain)
#epsilon     <- numeric(T)
#alpha       <- numeric(T)
#pred_errors <- integer(T)
#f_boost     <- integer(n)

result <- boostClassifier(T, Xtrain, Ytrain, n)
epsilon     <- as.vector( result[1] )
alpha       <- as.vector( result[2] )
pred_errors <- as.vector( result[3] )
p           <- as.data.frame( result[4] )
f_boost     <- as.vector( unlist(result[5]) )

result <- calculatePredictionAccuracy(n, Ytrain, f_boost)
pred_accuracy <- as.numeric( result[1] )
C <- result[2]
cat("\nTraining Accuracy:")
cat("\nNumber of test cases = ", n, 
    "\nPrediction accuracy = ", pred_accuracy, 
    "\nPrediction error = ", 1 - pred_accuracy, "\n")
cat("C = \n")
print(C)

## Plot training error as a function of iteration t
## Plot alpha_t and epsilon_t as a function of t

## Run on the test set
n <- length(Ytest)
#epsilon     <- numeric(T)
#alpha       <- numeric(T)
#pred_errors <- integer(T)
#f_boost     <- integer(n)

result <- boostClassifier(T, X, Y, n)
epsilon     <- as.vector( result[1] )
alpha       <- as.vector( result[2] )
pred_errors <- as.vector( result[3] )
p           <- as.data.frame( result[4] )
f_boost     <- as.vector( unlist(result[5]) )

result <- calculatePredictionAccuracy(n, Y, f_boost)
pred_accuracy <- as.numeric( result[1] )
C <- result[2]
cat("\nTesting Accuracy:")
cat("\nNumber of test cases = ", n, 
    "\nPrediction accuracy = ", pred_accuracy, 
    "\nPrediction error = ", 1 - pred_accuracy, "\n")
cat("C = \n")
print(C)

## Plot test error as a function of iteration t
## Plot alpha_t and epsilon_t as a function of t


## Indicate the testing accuracy by learning the Bayes classifier on the training set without boosting

