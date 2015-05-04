runNMF_EP <- function(X, k, T) {
        N1 <- nrow(X)
        N2 <- ncol(X)
        
        H <- matrix(runif(N2*k), nrow=k, ncol=N2, byrow=TRUE)
        W <- matrix(runif(N1*k), nrow=N1, ncol=k, byrow=TRUE)
        X <- as.matrix(X)
        L <- numeric(T)
        
        L[1] <- sum((X - (W %*% H))^2)
        cat("\nL = ", L[1])
        for (t in 1:T) {
                H <- H * ((t(W) %*% X) / ((t(W) %*% W) %*% H))
                W <- W * ((X %*% t(H))  / (W %*% H %*% t(H)))       
                L[t] <- sum((X - (W %*% H))^2)
                cat(', ', L[t]) 
        }
        return(list(L, W, H))
}

runNMF_DP <- function(X, k, T) {
        N1 <- nrow(X)
        N2 <- ncol(X)
        
        H <- matrix(runif(N2*k), nrow=k, ncol=N2, byrow=TRUE)
        W <- matrix(runif(N1*k), nrow=N1, ncol=k, byrow=TRUE)
        X <- as.matrix(X)
        L <- numeric(T)
        small_number <- 10^-16
        
        L[1] <- - ( sum( X * log( small_number + W %*% H ) - W %*% H) )
        cat("\nL = ", L[1])
        for (t in 2:T) {
                H <- H * normalizeRow( t(W)) %*% ( X / (small_number + W %*% H) )
                W <- W * ( X / (small_number + W %*% H) ) %*% normalizeColumn(t(H))
                                
                L[t] <- - ( sum( X * log( small_number + W %*% H ) - W %*% H) )
                cat(L[t], ", ")
        }
        return(list(L, W, H))
}

constructTermFreqMatrix <- function(docs, vocab) {
        ## X[i,j] is the number of times vocabulary word i appears in document j.
        docs <- as.matrix(docs)
        vocab <- as.matrix(vocab)
        X <- matrix(rep(0), nrow = nrow(vocab), ncol=nrow(docs))
        for (r in 1:nrow(docs)) {
                for (c in 1:ncol(docs)) {
                        idxcnt <- unlist( strsplit(docs[r,c], ":", fixed=FALSE) )
                        idx <- as.numeric( idxcnt[1] )
                        cnt <- as.numeric( idxcnt[2] )
                        j <- as.numeric(r)
                        X[idx, j] <- cnt
                }
        }
        return(X)
}

normalizeRow <- function(M) {
        t(apply( M, 1, function(x) (x/sum(x)) ))
}

normalizeColumn <- function(M) {
        apply( M, 2, function(x) (x/sum(x)) )   
}

rotateMatrix <- function(M) {        
        ## Rotates a matrix 90 degrees by reversing the row order and then taking the transpose
        t(M[nrow(M):1,,drop=FALSE])
}
