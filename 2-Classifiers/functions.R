classifyKNN <- function( k=1, X, Xtrain, Ytrain ) {        
        ## Compute Euclidean distance from X to each vector in Xtrain
        
        r <- nrow(Xtrain)
        d <- ncol(Xtrain)
        Xdist <- vector(mode="numeric", length=r)
        
        for (i in 1:r) {
                Xdist[i] <- sum( (X - Xtrain[i,])^2 )
        }
        Xdist <- sqrt(Xdist)
        
        ## Get the indices of the k-nearest neighbors
        kNN <- vector(mode="numeric", length=k)
        kNN <- which ( rank(Xdist, ties.method='random', na.last=TRUE) <= k )
        
        ## Return the class that appears most frequently amongst the k-NN;  
        ## table is an R fn that returns a contingency table, which gives us the frequencies 
        y  <- Ytrain[kNN]
        yf <- as.data.frame( table(y), stringsAsFactors=FALSE )
        ind  <- which ( yf$Freq == max(yf$Freq) )
        as.integer( yf[ind,1] )       
}

classifyBayes <- function( x, y, case, explore=FALSE ) {       
        class_MU      <- matrix(rep(0), nrow=1,  ncol=20)
        class_SIGMA   <- matrix(rep(0), nrow=20, ncol=20)
        class_SIGMA_I <- matrix(rep(0), nrow=20, ncol=20)
        classifier    <- vector("integer", 10)
        
        X  <- as.matrix(x)
        y  <- as.integer(y)
        n  <- nrow(Ytrain)
        p1 <- p2 <- p3 <- p4 <- 0
        for (k in 1:10) {
                class_prior   <- getPrior( class=k-1, n )  
                class_MU      <- as.matrix( getMu( class=k-1 ) ) 
                class_SIGMA   <- getSigma( class=k-1, t(class_MU) )
                class_SIGMA_I <- solve(class_SIGMA) 
                
                p1 <- -0.5 * t(X - class_MU)
                p2 <- p1 %*% class_SIGMA_I
                p3 <- p2 %*% (x - class_MU)
                p4 <- exp(p3)
                classifier[k] <- prod(class_prior, 1/sqrt(det(class_SIGMA)), p4)
        }
        winner <- as.integer( which (classifier == max(classifier)) )
        if ((winner-1) != y) {
                cat("\n", case, y, winner-1)
        }
        as.integer( winner-1 )
}

getMu <- function(class) {
        ## To Do: add caching
        
        g1  <- filter( training_set, y==class )
        MU <- as.matrix( summarise_each(g1, funs(mean)) )
        MU[1,-1]
}

getSigma <- function(class, class_mu) {
        ## To Do: add caching
        
        x <- as.matrix( filter( training_set, y==class )[,-1] )
        class_n <- nrow(x)
        d <- 20
        for ( j in 1:d ) {
                mu    <- class_mu[1,j]
                x[,j] <- x[,j] - mu
        }
        SIGMA <- as.matrix( t(x) %*% x )
        SIGMA <- SIGMA / class_n
        SIGMA
}

getPrior <- function(class, n) {
        ## To Do: add caching
        
        g1  <- filter( training_set, y==class )
        PI <- as.numeric( summarise(g1, p = n() / n) )
        PI
}

fit_softmax_wml <- function(X = Xtrain, Y = Ytrain) {     
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

predict_softmax_Y <- function(x, y, w = wml, case) {
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

visualizeX <- function( X, obs=10 ) {
        ## obs is the row number in X that you want to visualize        
        X <- as.matrix( X[obs,] )
        Y <- matrix( nrow=784, ncol=1 )
        Y <- Q %*% X
        Y <- matrix( Y, nrow=28, byrow=TRUE )
        image( rotateMatrix(Y) )
}

rotateMatrix <- function(M) {        
        ## Rotates a matrix 90 degrees by reversing the row order and then taking the transpose
        t(M[nrow(M):1,,drop=FALSE])
}

