
getMu <- function(class) {
## TO DO :  add caching
        i <- class+1
        g1  <- filter( training_set, y==class )
        g2  <- group_by( g1, y )
        MU[i,] <- as.matrix( summarise_each(g2, funs(mean)) )
        MU[i,-1]
}

getSigma <- function(class, class_mu) {
        i <- class+1
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
        i <- class+1
        g1  <- filter( training_set, y==class )
        g2 <- group_by( g1, y )
        PI[i,] <- as.numeric( summarise(g2, p = n() / n) )
        PI[i,]
}

classifyBayes <- function( x, y, case ) {       
        n <- nrow(Ytrain)
        class_SIGMA   <- matrix(nrow=20, ncol=20)
        class_SIGMA_I <- matrix(nrow=20, ncol=20)
        class_prior   <- vector("numeric", 2)
        classifier    <- vector("integer", 10)
        x <- as.matrix(x)
        for (k in 1:10) {
                class_prior   <- getPrior( class=k-1, n )  
                class_MU      <- as.matrix( getMu( class=k-1 ) ) 
                class_SIGMA   <- getSigma( class=k-1, class_MU )
                class_SIGMA_I <- solve(class_SIGMA) 
                class_MU      <- t(class_MU)
                
                p1 <- -0.5 * t(x - class_MU)
                p2 <- p1 %*% class_SIGMA_I
                p3 <- p2 %*% (x - class_MU)
                p4 <- exp(p3)
                classifier[k] <- prod(class_prior[2], 1/sqrt(det(class_SIGMA)), m4)
        }
        rm(list=c("p1", "p2", "p3", "p4"))       
        winner <- which (classifier == max(classifier))
        if ((winner-1) != y) {
                cat("\n", case, y, winner-1 )
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
        
        
        