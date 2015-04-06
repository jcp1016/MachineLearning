sampleDiscreteRV <- function(n, W) {
        ## Args: 
        ## n is a positive integer
        ## W is a discrete k-dimensional probability distribution
        ##
        ## Returns a vector of n integers whose distribution is the cdf of W
        RV <- integer(n)
        Fx <- cumsum(W)
        U  <- runif(n)
        for (i in 1:n) {
                RV[i] <- min( which(Fx >= U[i]) )
        }
        RV
}

boostClassifier <- function(T, X, Y) {
        ## T is the number of boosting iterations
        ## X is the training or test set
        ## Y are the training or test labels
        n           <- length(Y)
        p           <- matrix(rep(0), nrow=T, ncol=n)
        p[1,]       <- rep(1/n, n)
        Ypred       <- matrix(nrow=T, ncol=n)
        epsilon     <- numeric(T)
        alpha       <- numeric(T)
        pred_errors <- integer(T)
        pred_errors[1] <- 1
        f_boost     <- integer(n)
        X <- as.matrix(X)
        W <- matrix(rep(0), nrow=10, ncol=2) ## for online logistic regression classifier
        for (t in 1:T) {
                if (pred_errors[t] > 0) {
                        p[t,] <- p[t,] / sum(p[t,], na.rm=TRUE)
                        RV  <- sampleDiscreteRV(n, p[t,])
                        B_t <- training_set[RV,]
                        for (i in 1:n) {
                                W <- classifyOnline(B_t, 0.1)
                                fx <- t(X[i,]) %*% W
                                Ypred[t,i] <- sign( sum(fx) )
                                ##Ypred[t,i] <- classifyBayesLinear(B_t, Xtest[i,])
                        }
                        errors <- which( Ypred[t,] != Y )
                        epsilon[t] <- sum( p[t, errors] )
                        if (epsilon[t] == 0) {
                                next
                        }
                        alpha[t] <- 0.5 * log( (1-epsilon[t]) / epsilon[t] )
                        if (t < T) {
                                for (i in 1:n) {
                                        p[t+1,i] <- p[t,i] * exp(-alpha[t] * Y[i] * Ypred[t,i]) 
                                        f_boost[i] <- sign( sum(alpha[1:t] %*% Ypred[1:t,i], na.rm=TRUE) )
                                }
                                pred_errors[t+1] <- length( which( f_boost != Y ) )
                        }
                } 
        }
        return( list(epsilon, alpha, pred_errors, p, f_boost) )
}

calculatePredictionAccuracy <- function(n, Y, Ypred) {
        ## Assumes each Y value is -1 or 1
        C_names <- as.character(c(-1,1))
        C <- matrix( rep(0), nrow=2, ncol=2, dimnames=list(C_names, C_names)) 
        actual <- pred <- 0
        for (i in 1:n) {
                actual <- Y[i]
                pred   <- Ypred[i]
                if (actual == -1) { actual <- 0 }
                if (pred   == -1) { pred   <- 0 }
                C[actual+1, pred+1] <- C[actual+1, pred+1] + 1   
                actual <- pred <- 0
        }
        pred_accuracy <- 0
        if (n > 0) { pred_accuracy <- calcTrace(C) / n }
        return(list(pred_accuracy, C))
}

classifyBayesLinear <- function(B_t, X0) {
        ## B_t is a training set: col1 is the label, col2 is a bias term (1), and cols3,... are features;
        ## Gaussian parameters for the Bayes classifier are computed from B_t;
        ## X0 is the vector we are classifying
        nc  <- ncol(B_t)
        PI0 <- getPrior(-1, B_t)
        PI1 <- getPrior(1, B_t)
        MU0 <- as.matrix( getMu(-1, B_t)[-1] )
        MU1 <- as.matrix( getMu( 1, B_t)[-1] )
        MU  <- as.vector( getMu( 0, B_t)[-1] )
        SIGMA <- getSigma(MU, B_t[,3:nc]) ## exclude label and bias term from SIGMA
        SIGMA_I <- solve(SIGMA)

        w0 <- log(PI1/PI0) - 0.5 * t(MU1 + MU0)  ## (1x9)
        w0 <- w0 %*% SIGMA_I                     ## (1x9)
        w0 <- w0 %*% (MU1 - MU0)                 ## (1x1)

        w   <- SIGMA_I %*% (MU1 - MU0)           ## (9x1)
        w   <- rbind(w0,w)

        X0 <- as.matrix(X0)
        f_x <- X0 %*% w                          ## (1x1)
        sign(f_x)
}

getMu <- function(class, S) {
        if (class == -1 || class == 1) {
                S  <- filter(S, S$y==class)
                MU <- colMeans(S, na.rm=TRUE)
        } else {
                MU <- colMeans(S, na.rm=TRUE)
        }
        MU[-1]  ## return the x terms
}

getSigma <- function(MU, X) {
        n  <- nrow(X)
        X  <- as.matrix(X)
        MU <- as.matrix(MU)
        for ( j in 1:ncol(X) ) {
                mu    <- MU[j]
                X[,j] <- X[,j] - mu
        }
        SIGMA <- as.matrix( t(X) %*% X )
        SIGMA <- SIGMA / n
        SIGMA
}

getPrior <- function(class, sample) {
        n <- nrow(sample)
        g1 <- filter( sample, y==class )
        PI <- as.numeric( summarise(g1, p = n() / n) )
        PI
}

fitSoftmaxWML <- function(X = Xtrain, Y = Ytrain) {
        n <- length(Y)
        W <- matrix(rep(0), nrow=10, ncol=2)
        step <- 0.1/n
        #X <- cbind(1, X)

        ## For each class, estimate the optimal vector of coefficients;
        ind <- rep.int(0, n)
        for (t in 1:1000) {
                for (class in c(-1,1)) {
                        for (j in 1:n) {
                                ind[j] <- as.integer( Y[j] == class )
                        }
                        s   <- as.matrix( X %*% W )
                        p   <- exp( s[,class+1] ) / rowSums( exp(s) )
                        dw  <- as.data.frame( t(X) %*% (ind - p) )
                        W[,class+1] <- as.matrix( W[,class+1] + step * dw)
                        ind <- rep.int(0, n)
                }
                s <- as.matrix( X %*% W )
                L[t] <- sum(s) - log( sum(exp(s)) )
        }
        W
}

predictSoftmaxY <- function(x, y, w = wml, case) {
        X <- as.matrix(x)
        X <- rbind(1, X)
        W <- as.matrix(w)
        classifier <- t(W) %*% X
        winner <- which (classifier == max(classifier))
        as.integer( winner-1 )
}

classifyOnline <- function(S, eta=0.1) {
        ## S is a data set of size (n x 11)
        n <- nrow(S)
        W <- matrix(rep(0), nrow=10, ncol=2)
        S <- S[sample(n),]  ## randomly order the data row-wise
        Y <- as.vector( S[,1] )
        X <- as.matrix( S[,-1] )
        Xt <- t(X)
        
        for (i in 1:n) {
                x <- as.matrix( X[i,] )      ## (10x1)
                xt <- t(x)                   ## (1x10)
                yx <- as.matrix( Y[i] * x )  ## (10x1)
                w  <- as.matrix( W )         ## (10x2)
                wt <- t(w)                   ## (2x10)

                s1 <- as.matrix( xt %*% w )       ## (1x2) 
                s1 <- -Y[i] * s1                   ## (1x2)
                sigma <- 1 / (1 + exp(s1) )       ## (1x2)
                s3 <- eta * (1 - sigma)           ## (1x2)
                s4 <- yx %*% s3                   ## (10x2)
                W <- W + s4                       ## (10x2)
        }
        W
}

calcTrace <- function(M) {
        ## Assumes M is a square matrix
        r <- nrow(M)
        Mtrace <- 0
        for (i in 1:r) {
                Mtrace <- Mtrace + M[i,i]
        }
        Mtrace
}