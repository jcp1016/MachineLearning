require("ggplot2")

## Read inputs
setwd("./cancer_csv")
X <- read.csv("X.csv", header=FALSE)
Y <- read.csv("y.csv", header=FALSE)

test <- c(1:183)
Xtest <- X[test,]
Ytest <- as.data.frame( Y[test,] )
Xtrain <- X[-test,]
Ytrain <- as.data.frame( Y[-test,] )

## Part 1 
W <- c(0.1, 0.2, 0.3, 0.4)
for (n in c(100, 200, 300, 400)) { 
        RV <- sampleDiscreteRV(n, W)
        ggplot(as.data.frame(RV), aes(x=RV)) + 
                geom_histogram(binwidth=1, color="black", fill="white") +
                scale_x_discrete()
        fn <- paste0("hist",n,".png")
        ggsave(filename=fn)
}

## Part 2
T <- 1000
n <- nrow(Ytrain)
d <- ncol(Xtrain)
W <- rep(1/n, n)
training_set <- cbind(Ytrain, Xtrain)
names(training_set)[1] <- "y"

## define the confusion matrix
CMnames <- as.character(c(1,-1))  
CM      <- matrix( rep(0), nrow=d, ncol=d, byrow=TRUE, dimnames=list(CMnames, CMnames))

## make predictions on the test set;  calculate Gaussian parameters from the bootstrap sample 
Ypred   <- integer(n)
for (t in 1:T) {
        ## get bootstrap sample
        RV <- sampleDiscreteRV(n, w)
        Bt <- training_set[RV,]
        CM <- matrix( rep(0), nrow=10, ncol=10, byrow=TRUE, dimnames=list(CMnames, CMnames))
        for (i in 1:n) {
                Ypred[i] <- classifyBayesLinear(Bt, Xtest[i,], Ytest[i], i)
                CM[Ytest[i]+1, Ypred[i]+1] <- CM[Ytest[i]+1, Ypred[i]+1] + 1    
        }
        pred_accuracy <- 0
        if (n > 0) {
                pred_accuracy <- calcTrace(C) / n
        }
}
