genDataFromGaussMixture <- function(d, N, MU, SIGMA, PI) {
        ## Generate N observations in R^d from a pre-defined mixture of Gaussians
        data <- matrix(rep(0), nrow=N, ncol=d)
        sd <- as.vector( sqrt(diag(SIGMA)) )
        Fx <- cumsum(PI)
        U  <- runif(N)
        for (i in 1:N) {
                g <- min( which(Fx >= U[i]) )
                mu <- unlist(MU[g])
                data[i,] <- rnorm( d, mu, sd )
        }
        data
}

assignToCluster <- function(X, mu, K) {
        N  <- nrow(X)
        d  <- ncol(X)
        CK <- matrix(rep(0), nrow=N, ncol=K)
        CN <- numeric(N)  
        for (k in 1:K) {
                m <- mu[k]
                for (n in 1:N) {
                        CK[n,k] <- sum((X[n,] - m)^2)
                }
        }
        for (n in 1:N) {
                argmin <- min(CK[n,])
                CN[n] <- suppressWarnings(min( which(CK[n,] == argmin))) ## min in case there are dups
        }
        return(CN)
}

calcObjFunction <- function(X, MU, C, K) {
        N <- nrow(X)
        d <- ncol(X)
        L <- 0
        for (i in 1:N) {
                for (k in 1:K) {
                        ind <- which(C == k)
                        r <- length(ind)
                        if (r > 0) {
                                Xk <- matrix(rep(0), nrow=r, ncol=d)
                                Xk <- X[ind, ]
                                mu <- MU[k]
                                L <- L + sum((Xk - mu)^2)
                        }
                }
        }
        return(L)
}

updateCentroids <- function(X, c, K, t) {
        M <- numeric(K)
        for (k in 1:K) {
                ind <- which(c == k)
                M[k] <- mean(X[ind,])
        }
        M   
}
