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
tr_n <- length(Ytrain)
result3         <- boostClassifier(T, Xtrain, Ytrain, tr_n)
tr_epsilon     <- as.vector( unlist(result3[1]) )
tr_alpha       <- as.vector( unlist(result3[2]) )
tr_pred_errors <- as.vector( unlist(result3[3]) )
tr_p           <- as.data.frame( result3[4] )
tr_f_boost     <- as.vector( unlist(result3[5]) )
result4 <- calculatePredictionAccuracy(tr_n, Ytrain, tr_f_boost)
tr_pred_accuracy <- as.numeric( result4[1] )
tr_C <- result4[2]
cat("\nTraining Accuracy:")
cat("\nNumber of test cases = ", n, 
    "\nPrediction accuracy = ", tr_pred_accuracy, 
    "\nPrediction error = ", 1 - tr_pred_accuracy, "\n")
cat("C = \n")
print(tr_C)

## Run on the test set
te_n <- length(Ytest)
result1        <- boostClassifier(T, Xtest, Ytest, te_n)
te_epsilon     <- as.vector( unlist(result1[1]) )
te_alpha       <- as.vector( unlist(result1[2]) )
te_pred_errors <- as.vector( unlist(result1[3]) )
te_p           <- as.data.frame( result1[4] )
te_f_boost     <- as.vector( unlist(result1[5]) )
result2 <- calculatePredictionAccuracy(te_n, Ytest, te_f_boost)
te_pred_accuracy <- as.numeric( result2[1] )
te_C <- result2[2]
cat("\nTesting Accuracy:")
cat("\nNumber of test cases = ", te_n, 
    "\nPrediction accuracy = ", te_pred_accuracy, 
    "\nPrediction error = ", 1 - te_pred_accuracy, "\n")
cat("C = \n")
print(te_C)

## Plot training and testing error as a function of t
t <- c(1:T)
p1 <- as.data.frame(tr_pred_errors)
p1 <- p1 / tr_n
names(p1) <- "training_error"
p2 <- as.data.frame(te_pred_errors)
p2 <- p2 / te_n
pdata <- cbind(p1, p2)
names(pdata)[2] <- "testing_error"
pdata <- cbind(pdata, t)
pdata <- pdata[-1,]  ## remove initialized values
ggplot(pdata) +
        geom_point(shape=19, position="identity", alpha=0.5,
                   aes(x=pdata$t, y=pdata$training_error, colour="Training error")) +
        geom_point(shape=19, position="identity", alpha=0.5,
                   aes(x=pdata$t, y=pdata$testing_error, colour="Testing error")) +
        theme_bw() + scale_fill_hue() + xlab("t") + ylab("") +
        theme(legend.title=element_blank()) + 
        ggtitle("Prediction error at iteration t")
#ggsave(filename="prediction_error.png")

## Plot alpha as a function of t
pdata <- as.data.frame(te_alpha)
pdata$tr_alpha <- tr_alpha
pdata$t <- c(1:T)
ggplot(pdata) +
        geom_point(shape=19, position="identity", alpha=0.5,ggplot(pdata) + 
                   aes(x=pdata$t, y=pdata$tr_alpha)) +
        theme_bw() + xlab("t") + ylab("") + 
        theme(legend.title=element_blank()) +
        ggtitle("Training alpha at iteration t")
#ggsave(filename="training_alpha.png")

ggplot(pdata) +
        geom_point(shape=19, position="identity", alpha=0.5,
                   aes(x=pdata$t, y=pdata$te_alpha)) +
        theme_bw() + xlab("t") + ylab("") +
        theme(legend.title=element_blank()) +
        ggtitle("Testing alpha at iteration t")
#ggsave(filename="testing_alpha.png")

## Plot epsilon as a function of t
pdata <- as.data.frame(te_epsilon)
pdata$tr_epsilon <- tr_epsilon
pdata$t <- c(1:T)
ggplot(pdata) +
        geom_point(shape=19, position="identity", alpha=0.5,
                   aes(x=pdata$t, y=pdata$tr_epsilon)) +
        theme_bw() + xlab("t") + ylab("") +
        theme(legend.title=element_blank()) +
        ggtitle("Training epsilon at iteration t")
ggsave(filename="training_epsilon.png")

ggplot(pdata) +
        geom_point(shape=19, position="identity", alpha=0.5,
                   aes(x=pdata$t, y=pdata$te_epsilon)) +
        theme_bw() + xlab("t") + ylab("") +
        theme(legend.title=element_blank()) +
        ggtitle("Testing epsilon at iteration t")
ggsave(filename="testing_epsilon.png")

## Indicate the testing accuracy by learning the Bayes classifier on the training set without boosting
n <- length(Ytest)
for (i in 1:n) {
        Ypred[i] <- classifyBayesLinear(training_set, Xtest[i,])
}
result <- calculatePredictionAccuracy(n, Ytest, Ypred)
pred_accuracy <- as.numeric( result[1] )
C <- result[2]
cat("\nTesting Accuracy:")
cat("\nNumber of test cases = ", n, 
    "\nPrediction accuracy = ", pred_accuracy, 
    "\nPrediction error = ", 1 - pred_accuracy, "\n")
cat("C = \n")
print(C)

## Plot p as a function of t for three data points
pdata <- as.data.frame(te_p[,11])
names(pdata)[1] <- "p1"
pdata$p2 <- te_p[,13]
pdata$p3 <- tr_p[,400]
pdata$t <- c(1:1000)
ggplot(pdata) + 
        geom_point(shape=19, position="identity", alpha=0.5, aes(x=pdata$t, y=pdata$p1)) +
        theme_bw() + xlab("t") + ylab("") + theme(legend.title=element_blank()) +
        scale_fill_hue() + ggtitle("obs 11 at iteration t")
ggsave(filename="part2_p1.png")
ggplot(pdata) + 
        geom_point(shape=19, position="identity", alpha=0.5, aes(x=pdata$t, y=pdata$p2)) +
        theme_bw() + xlab("t") + ylab("") + theme(legend.title=element_blank()) +
        scale_fill_hue() + ggtitle("obs 13 at iteration t")
ggsave(filename="part2_p2.png")
ggplot(pdata) + 
        geom_point(shape=19, position="identity", alpha=0.5, aes(x=pdata$t, y=pdata$p3)) +        
        theme_bw() + xlab("t") + ylab("") + theme(legend.title=element_blank()) +
        scale_fill_hue() + ggtitle("obs 400 at iteration t")
ggsave(filename="part2_p3.png")

## Part 3
## Run on the training set
##tr_n <- length(Ytrain)
result1         <- boostClassifier(T, Xtrain, Ytrain, 500)
tr_epsilon     <- as.vector( unlist(result1[1]) )
tr_alpha       <- as.vector( unlist(result1[2]) )
tr_pred_errors <- as.vector( unlist(result1[3]) )
tr_p           <- as.data.frame( result1[4] )
tr_f_boost     <- as.vector( unlist(result1[5]) )
result2 <- calculatePredictionAccuracy(tr_n, Ytrain, tr_f_boost)
tr_pred_accuracy <- as.numeric( result2[1] )
tr_C <- result2[2]
cat("\nTraining Accuracy:")
cat("\nNumber of test cases = ", tr_n, 
    "\nPrediction accuracy = ", tr_pred_accuracy, 
    "\nPrediction error = ", 1 - tr_pred_accuracy, "\n")
cat("C = \n")
print(tr_C)

## Run on the test set
te_n <- length(Ytest)
result3        <- boostClassifier(T, Xtest, Ytest, te_n)
te_epsilon     <- as.vector( unlist(result3[1]) )
te_alpha       <- as.vector( unlist(result3[2]) )
te_pred_errors <- as.vector( unlist(result3[3]) )
te_p           <- as.data.frame( result3[4] )
te_f_boost     <- as.vector( unlist(result3[5]) )
result4 <- calculatePredictionAccuracy(te_n, Ytest, te_f_boost)
te_pred_accuracy <- as.numeric( result4[1] )
te_C <- result4[2]
cat("\nTesting Accuracy:")
cat("\nNumber of test cases = ", te_n, 
    "\nPrediction accuracy = ", te_pred_accuracy, 
    "\nPrediction error = ", 1 - te_pred_accuracy, "\n")
cat("C = \n")
print(te_C)