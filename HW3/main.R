require("ggplot2")
require("dplyr")

## Read inputs
setwd("./cancer_csv")
X <- read.csv("X.csv", header=FALSE)
Y <- read.csv("y.csv", header=FALSE)
setwd("../")

test   <- c(1:183)
Xtest  <- X[test,]
Ytest  <- as.vector( Y[test,] )
Xtrain <- X[-test,]
Ytrain <- as.vector( Y[-test,] )
training_set <- cbind(Ytrain, Xtrain)
testing_set  <- cbind(Ytest, Xtest)
names(training_set)[1] <- "y"
names(testing_set)[1]  <- "y"
n_test  <- length(Ytest)
n_train <- length(Ytrain)
## Define the confusion matrix
Cnames <- as.character(c(-1,1))
CM <- matrix( rep(0), nrow=2, ncol=2, byrow=TRUE, dimnames=list(Cnames, Cnames))

## Part 1
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
## Gaussian parameters are calculated from "boostrap" samples B_t.
T <- 500 
## Run on the test set
n <- n_test
Ypred <- matrix(nrow=n, ncol=T)
p <- numeric(n)
p <- rep(1/n, n)
epsilon <- numeric(T)
alpha   <- numeric(T)
f_boost <- integer(n)
for (t in 1:T) {
        RV  <- sampleDiscreteRV(n, p)
        B_t <- testing_set[RV,]
        for (i in 1:n) {
                Ypred[i,t] <- classifyBayesLinear(B_t, Xtest[i,], Ytest[i])
        }
        errors <- which( Ypred[,t] != Ytest )
        epsilon[t] <- sum( p[errors] )
        alpha[t] <- 0.5 * log( (1-epsilon[t]) / epsilon[t] )
        if (alpha[t] == 0) {
                break
        }
        for (i in 1:n) {
                p[i] <- p[i] * exp( -alpha[t] * Ytest[i] * Ypred[i,t] ) 
        }
        p <- p / sum(p, na.rm=TRUE)
}
A <- as.matrix(alpha)
Y <- as.matrix(Ypred)
Y <- t(Y)
for (i in 1:n) {
        f_boost[i] <- sign( t(A) %*% Y[,i] )
}
CM <- matrix( rep(0), nrow=2, ncol=2, byrow=TRUE, dimnames=list(Cnames, Cnames))
for (i in 1:n) {
        r <- Ytest[i]
        c <- f_boost[i]
        if (r == -1) r <- 0
        if (c == -1) c <- 0
        CM[r+1, c+1] <- CM[r+1, c+1] + 1    
}
pred_accuracy <- 0
if (n > 0) {
        pred_accuracy <- calcTrace(CM) / n
}
## Show results
cat("\nNumber of test cases = ", n, "\nPrediction accuracy = ", pred_accuracy, "\n")
cat("CM = \n")
print(CM)

## Repeat on the full training set


## Plot training and test error as a function of iteration t



## Indicate the testing accuracy by learning the Bayes classifier on the training set without boosting



## Plot alpha_t and epsilon_t as a function of t
