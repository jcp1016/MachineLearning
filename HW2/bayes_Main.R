require("dplyr")
require("devtools")
require("stats")
require("graphics")

## Set up inputs
setwd(".")
Xtrain <- as.matrix(read.table("./mnist_csv/Xtrain.txt", sep=',', dec='.',
                               check.names=FALSE, fill=FALSE, blank.lines.skip=TRUE))

Ytrain <- as.matrix(read.table("./mnist_csv/label_train.txt", sep=',', dec='.', 
                     check.names=FALSE, fill=FALSE, blank.lines.skip=TRUE))
Ytrain[,1] <- as.integer(Ytrain[,1])

Xtest <- as.matrix(read.table("./mnist_csv/Xtest.txt", sep=',', dec='.', 
                              check.names=FALSE, fill=FALSE, blank.lines.skip=TRUE))

Ytest <- as.matrix(read.table("./mnist_csv/label_test.txt", sep=',', dec='.', 
                    check.names=FALSE, fill=FALSE, blank.lines.skip=TRUE))
Ytest[,1] <- as.integer(Ytest[,1])

Q <- as.matrix(read.table("./mnist_csv/Q.txt", sep=',', dec='.', 
                check.names=FALSE, fill=FALSE, blank.lines.skip=TRUE))

source("bayes_Functions.R")
training_set <- as.data.frame( cbind(Ytrain, Xtrain) )
test_set     <- as.data.frame( cbind(Ytest,  Xtest) )
names(training_set)[1] <- "y"
names(test_set)[1]     <- "y"

## Define the confusion matrix
Cnames <- as.character(c(0:9))
C <- matrix( rep(0), nrow=10, ncol=10, byrow=TRUE, dimnames=list(Cnames, Cnames))

## Classify the test set and populate the confusion matrix
n <- nrow(Ytest)
Ypred  <- vector(mode="integer", length=n)
cat("\nMisclassified case, Ytest, Ypred")
for (i in 1:n) {
        Ypred[i] <- classifyBayes( Xtest[i,], Ytest[i], i )
        C[Ytest[i]+1, Ypred[i]+1] <- C[Ytest[i]+1, Ypred[i]+1] + 1    
}
pred_accuracy <- 0
if (n > 0) {
        pred_accuracy <- calcTrace(C) / n
}

## Show results
cat("\nNumber of test cases = ", n, "\nPrediction accuracy = ", pred_accuracy, "\n")
cat("C = \n")
print(C)
