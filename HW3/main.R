require("ggplot2")
require("ggthemes")
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
training_set <- cbind(Ytrain, Xtrain)
names(training_set)[1] <- "y"

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
result3         <- boostClassifier(T, Xtrain, Ytrain, n)
tr_epsilon     <- as.vector( unlist(result3[1]) )
tr_alpha       <- as.vector( unlist(result3[2]) )
tr_pred_errors <- as.vector( unlist(result3[3]) )
tr_p           <- as.data.frame( result3[4] )
tr_f_boost     <- as.vector( unlist(result3[5]) )

result4 <- calculatePredictionAccuracy(n, Ytrain, tr_f_boost)
tr_pred_accuracy <- as.numeric( result4[1] )
tr_C <- result4[2]
cat("\nTraining Accuracy:")
cat("\nNumber of test cases = ", n, 
    "\nPrediction accuracy = ", tr_pred_accuracy, 
    "\nPrediction error = ", 1 - tr_pred_accuracy, "\n")
cat("C = \n")
print(tr_C)

## Run on the test set
n <- length(Ytest)
result1        <- boostClassifier(T, Xtest, Ytest, n)
te_epsilon     <- as.vector( unlist(result1[1]) )
te_alpha       <- as.vector( unlist(result1[2]) )
te_pred_errors <- as.vector( unlist(result1[3]) )
te_p           <- as.data.frame( result1[4] )
te_f_boost     <- as.vector( unlist(result1[5]) )

result2 <- calculatePredictionAccuracy(n, Ytest, te_f_boost)
te_pred_accuracy <- as.numeric( result2[1] )
te_C <- result2[2]
cat("\nTesting Accuracy:")
cat("\nNumber of test cases = ", n, 
    "\nPrediction accuracy = ", te_pred_accuracy, 
    "\nPrediction error = ", 1 - te_pred_accuracy, "\n")
cat("C = \n")
print(te_C)

## Plot training and testing error as a function of t
t <- c(1:T)
p1 <- as.data.frame(tr_pred_errors)
p2 <- p1 / tr_n
error_type <- rep("Training error", 1000)
p3 <- cbind(error_type, p2)
p4 <- cbind(p3, t)
names(p4)[2] <- "prediction_error"

p5 <- as.data.frame(te_pred_errors)
p6 <- p5 / te_n
error_type <- rep("Testing error", 1000)
p7 <- cbind(error_type, p6)
p8 <- cbind(p7, t)
names(p8)[2] <- "prediction_error"

allp_error <- rbind(p4, p8)

ggplot(p8, aes(x=t, y=prediction_error, group=error_type, colour=error_type)) +
        geom_point(shape=19, position="identity", alpha=0.5) +
        scale_color_hue() +
        theme_bw() +
        ggtitle("Testing error at iteration t")
ggsave(filename="prediction_error.png")

## Plot alpha and epsilon as a function of t


## Indicate the testing accuracy by learning the Bayes classifier on the training set without boosting

