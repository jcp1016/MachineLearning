require("ggplot2")
require("dplyr")

## Read inputs
setwd("./cancer_csv")
X <- read.csv("X.csv", header=FALSE)
Y <- read.csv("y.csv", header=FALSE)
setwd("../")

test <- c(1:183)
Xtest <- X[test,]
Ytest <- as.vector( Y[test,] )
Xtrain <- X[-test,]
Ytrain <- as.vector( Y[-test,] )
training_set <- cbind(Ytrain, Xtrain)
testing_set  <- cbind(Ytest, Xtest)
names(training_set)[1] <- "y"
names(testing_set)[1]  <- "y"
CMnames <- c("-1","1")

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
T <- 1000
n_test  <- nrow(Ytest)
n_train <- nrow(Ytrain)

## Run on the test set
n <- n_test
p <- numeric(n)
p[1] <- 1/n
W <- rep(1/n, n)
Ypred <- integer(n)

for (t in 1:T) {
        RV <- sampleDiscreteRV(n, W)
        B_t <- training_set[RV,]
        CM <- matrix( rep(0), nrow=2, ncol=2, byrow=TRUE, dimnames=list(CMnames, CMnames))
        for (i in 1:n) {
                Ypred[i] <- classifyBayesLinear(B_t, Xtest[i,], Ytest[i])
                CM[Ytest[i]+1, Ypred[i]+1] <- CM[Ytest[i]+1, Ypred[i]+1] + 1
        }
        pred_accuracy[t] <- 0
        if (n > 0) {
                pred_accuracy[t] <- calcTrace(CM) / n
        }

}

## Repeat on the full training set


## Plot training and test error as a function of iteration t



## Indicate the testing accuracy by learning the Bayes classifier on the training set without boosting



## Plot alpha_t and epsilon_t as a function of t
