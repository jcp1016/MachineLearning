sampleDiscreteRV <- function(n, W) {
        ## Assumes n is a positive integer
        ## Assumes W is a discrete k-dimensional probability distribution
        RV <- integer(n)
        Fx <- cumsum(W)
        U  <- runif(n)
        for (i in 1:n) {
                RV[i] <- min( which(Fx >= U[i]) )
        }
        RV
}

classifyBayesLinear <- function(B_t, X, y) {
        ## B_t is a sample where col1 is the label, col2 is a bias term (1), and cols3,... are features;
        ## Gaussian parameters for the Bayes classifier are computed from B_t
        nc  <- ncol(B_t)
        PI0 <- getPrior(-1, B_t)
        PI1 <- getPrior(1, B_t)
        MU0 <- as.matrix( getMu(-1, B_t)[-1] )
        MU1 <- as.matrix( getMu( 1, B_t)[-1] )
        MU  <- as.vector( getMu( 0, B_t)[-1] )
        SIGMA <- getSigma(MU, B_t[,3:nc]) ## exclude label and bias term from SIGMA
        SIGMA_I <- solve(SIGMA)

        w0 <- 0
        w0 <- log(PI1/PI0) - 0.5 * t(MU1 + MU0)  ## (1x9)
        w0 <- w0 %*% SIGMA_I                     ## (1x9)
        w0 <- w0 %*% (MU1 - MU0)                 ## (1x1)

        w   <- SIGMA_I %*% (MU1 - MU0)           ## (9x1)
        w   <- rbind(w0,w)

        X <- as.matrix(X)
        f_x <- X %*% w                           ## (1x1)
        f_x
}

getMu <- function(class, S) {
        if (class == -1 || class == 1) {
                S  <- filter(S, S$y==class)
                MU <- colMeans(S, na.rm=TRUE)
        } else {
                MU <- colMeans(S, na.rm=TRUE)
        }
        MU[-1]
}

getSigma <- function(MU, X) {
        n  <- nrow(X)
        X  <- as.matrix(X)
        X1 <- X
        MU <- as.matrix(MU)
        for ( j in 1:ncol(X) ) {
                mu    <- MU[j]
                X[,j] <- X[,j] - MU[j]
        }
        SIGMA <- as.matrix( t(X) %*% X )
        SIGMA <- SIGMA / n
        SIGMA
}

getPrior <- function(class, sample) {
        ## To Do: add caching
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
        ## X is (n x 21),  W is (21 x 10), L is (21 x 1)
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
        if ((winner-1) != y) {
                #cat("\n", case, y, winner-1 )
        }
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
