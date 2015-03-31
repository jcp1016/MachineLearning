fitOnlineClassifier <- function(B_t, step=0.1, X, Y) {
        n <- length(Y)
        W <- matrix(rep(0), nrow=length(Y), ncol(X))
        X <- X[sample(nrow(X)),]  ## randomly order the data row-wise
        X <- as.matrix(X)
        Xt <- t(X)
        
        for (i in 1:n) {
                if (i < n) {
                        s <-  X[i,] %*% t(W) 
                        s <- s * -Y[i] 
                        s <- 1 / (1 + exp(s))
                        s2 <- (step * (1-s) * Y[i]) 
                        s3 <- s2 %*% Xt[i,]
                        W[i+1,] <- W[i,] + s2
        }
        #s <- as.matrix( X %*% W )
        #L[t] <- sum(s) - log( sum(exp(s)) )
        }
        W
}