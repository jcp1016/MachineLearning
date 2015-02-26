
classify <- function( k=1, X, Xtrain, Ytrain ) {        
        ## Compute Euclidean distance from X to each vector in Xtrain
        #r <- as.numeric( nrow(Xtrain) )
        r <- nrow(Xtrain)
        d <- ncol(Xtrain)
        Xdist <- vector(mode="numeric", length=r)
        
        for (i in 1:r) {
                Xdist[i] <- sum( (X - Xtrain[i,])^2 )
        }
        Xdist <- sqrt(Xdist)
        
        ## Get the indices of the k-nearest neighbors
        #k   <- as.numeric(k)
        kNN <- vector(mode="numeric", length=k)
        kNN <- which ( rank(Xdist, ties.method='random', na.last=TRUE) <= k )
        
        ## Return the class that appears most frequently amongst the k-NN;  
        ## table is an R fn that returns a contingency table, which gives us the frequencies 
        y  <- Ytrain[kNN]
        yf <- as.data.frame( table(y), stringsAsFactors=FALSE )
        ind  <- which ( yf$Freq == max(yf$Freq) )
        as.integer( yf[ind,1] )       
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
 
visualizeX <- function( Q=Q, X=Xtrain, obs=10 ) {
        ## obs is the row number in X that you want to visualize        
        X <- as.matrix( X[obs,] )
        Y <- matrix( nrow=784, ncol=1 )
        Y <- Q %*% X
        Y <- matrix( Y, nrow=28, byrow=TRUE )
        image( rotateMatrix(Y) )
}

rotateMatrix <- function(M) {        
        ## Rotates a matrix 90 degrees by reversing the row order and then taking the transpose
        Mrev <- M[nrow(M):1,,drop=FALSE]
        t(Mrev)
}
        
        
        