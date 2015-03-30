fitOnlineClassifier <- function(B_t, step=0.1) {
        
        n <- length(Ytrain)
        X <- cbind(1, Xtrain)
        
        W <- matrix(rep(0), nrow=n, ncol=ncol(X))
        Xr <- X[sample(nrow(X)),]  ## randomly order the data row-wise
                
        for (i in 1:n) {
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