require("devtools")
require("stats")
require("graphics")

## Read the data
setwd(".")
Xtrain <- as.matrix(read.table("./mnist_csv/Xtrain.txt", sep=',', dec='.',
                               check.names=FALSE, fill=FALSE, blank.lines.skip=TRUE))

Ytrain <- as.matrix(read.table("./mnist_csv/label_train.txt", sep=',', dec='.', 
                     check.names=FALSE, fill=FALSE, blank.lines.skip=TRUE))

Xtest <- as.matrix(read.table("./mnist_csv/Xtest.txt", sep=',', dec='.', 
                              check.names=FALSE, fill=FALSE, blank.lines.skip=TRUE))
Ytest <- as.matrix(read.table("./mnist_csv/label_test.txt", sep=',', dec='.', 
                    check.names=FALSE, fill=FALSE, blank.lines.skip=TRUE))

Q <- as.matrix(read.table("./mnist_csv/Q.txt", sep=',', dec='.', 
                check.names=FALSE, fill=FALSE, blank.lines.skip=TRUE))

source("knn_Functions.R")

#Visualize a row of Xtrain or Xtest
#p1 <- visualizeX( Q, Xtrain, 4000 )

n <- nrow(Xtest)
Ypred <- vector(mode="integer", length=n)
options(warn=-1)
for (k in 1:5) {
        #C is the confusion matrix
        Cnames <- as.character(c(0:9))
        C <- matrix( rep(0), nrow=10, ncol=10, byrow=TRUE, dimnames=list(Cnames, Cnames))
        for (i in 1:n) {
                Ypred[i] <- classifyKNN( k, Xtest[i,], Xtrain, Ytrain )
                C[Ytest[i]+1, Ypred[i]+1] <- C[Ytest[i]+1, Ypred[i]+1] + 1
        }
        pred_accuracy <- calcTrace(C) / n
        cat("\nk = ",k, "\nPrediction accuracy = ", pred_accuracy, "\n")
        cat("C = \n")
        print(C)
}
options(warn=0)


