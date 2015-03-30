sampleDiscreteRV <- function(n, W) {
        ## Assumes n is a positive integer,
        ## and W is a discrete k-dimensional probability distribution
        RV <- integer(n)
        Fx <- cumsum(W)
        U  <- runif(n)
        for (i in 1:n) {
                RV[i] <- min( which(Fx >= U[i]) )
        }
        RV
}

boostClassifier <- function(T, Xtest, Ytest, n) {
        p           <- matrix(rep(0), nrow=T, ncol=n)
        p[1,]       <- rep(1/n, n)
        Ypred       <- matrix(nrow=T, ncol=n)
        epsilon     <- numeric(T)
        alpha       <- numeric(T)
        pred_errors <- integer(T)
        pred_errors[1] <- 1
        f_boost     <- integer(n)
        for (t in 1:T) {
                if (pred_errors[t] > 0) {
                        p[t,] <- p[t,] / sum(p[t,], na.rm=TRUE)
                        RV  <- sampleDiscreteRV(n, p[t,])
                        B_t <- training_set[RV,]
                        for (i in 1:n) {
                                Ypred[t,i] <- classifyBayesLinear(B_t, Xtest[i,])
                        }
                        errors <- which( Ypred[t,] != Ytest )
                        epsilon[t] <- sum( p[t, errors] )
                        if (epsilon[t] == 0) {
                                next
                        }
                        alpha[t] <- 0.5 * log( (1-epsilon[t]) / epsilon[t] )
                        if (t < T) {
                                for (i in 1:n) {
                                        p[t+1,i] <- p[t,i] * exp(-alpha[t] * Ytest[i] * Ypred[t,i]) 
                                        f_boost[i] <- sign( sum(alpha[1:t] %*% Ypred[1:t,i], na.rm=TRUE) )
                                }
                                pred_errors[t+1] <- length( which( f_boost != Ytest ) )
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
                if (actual == -1) actual <- 0
                if (pred   == -1) pred   <- 0
                C[actual+1, pred+1] <- C[actual+1, pred+1] + 1   
                actual <- pred <- 0
        }
        pred_accuracy <- 0
        if (n > 0) {
                pred_accuracy <- calcTrace(C) / n
        }
        return(list(pred_accuracy, C))
}

classifyBayesLinear <- function(B_t, X0) {
        ## B_t is a sample from the training set: col1 is the label, col2 is a bias term (1), and cols3,... are features;
        ## Gaussian parameters for the Bayes classifier are computed from B_t;
        ## X0 is the vector that we are classifying
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
        W <- matrix(rep(0), nrow=21, ncol=10)
        n <- nrow(X)
        step <- 0.1/n
        X <- cbind(1, X)

        ## For each class, estimate the optimal vector of coefficients;
        ind <- rep.int(0, n)
        for (t in 1:1000) {
                for (class in 0:9) {
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

calcTrace <- function(M) {
        ## Assumes M is a square matrix
        r <- nrow(M)
        Mtrace <- 0
        for (i in 1:r) {
                Mtrace <- Mtrace + M[i,i]
        }
        Mtrace
}
