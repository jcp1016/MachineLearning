calculateU <- function(i) {
        u   <- matrix(rep(0), nrow=1, ncol=d, byrow=TRUE)
        p2  <- matrix(rep(0), nrow=d, ncol=d, byrow=TRUE)
        ind <- which( OMEGA_u[i,] == 1 )
        p2  <- t(V[ind,]) %*% V[ind,] # 20x20
        p3  <- p1 + p2
        p3  <- solve(p3) #20x20
        iratings <- unlist(filter(ratings, user_id==i)[3])  ## all of user i's movie ratings
        M <- matrix(rep(0), nrow=1, ncol=N2, byrow=TRUE)
        k <- 1
        for (j in ind) {
                rating <- iratings[k]
                M[,j] <- rating
                k <- k+1
        }
        p4 <- M[1,] %*% V
        u[1,] <- p4 %*% p3 ## 1xd
        l1 <- u[1,] %*% t(V)
        L_ti <<- sum(L_ti, sum((M[1,] - l1)^2))
        u
}

calculateV <- function(j) {
        v   <- matrix(rep(0), nrow=1, ncol=d, byrow=TRUE)
        p2  <- matrix(rep(0), nrow=d, ncol=d, byrow=TRUE)
        ind <- which( OMEGA_v[j,] == 1 )
        if (is.matrix(U[ind,])) {
                p2 <- t(U[ind,]) %*% U[ind,]
        } else {
                p2 <- U[ind,] %*% t(U[ind,])
        }
        p3 <- solve(p1 + p2)
        iratings <- unlist(filter(ratings, movie_id==j)[3]) ## all users who rated movie j
        M <- matrix(rep(0), nrow=1, ncol=N1, byrow=TRUE)
        k <- 1
        for (i in ind) {
                rating <- iratings[k]
                M[,i] <- rating
                k <- k+1
        }
        p4 <- M[1,] %*% U
        v[1,] <- p4 %*% p3 ## 1xd
        v
}

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
        list(as.integer( yf[ind,1] ), Xdist[kNN])
}

recommendMoviesByDistance <- function(s, k) {
        X <- V[s,]
        result <- classifyKNN(k+1, X, V, movies[,1])
        m <- unlist(result[1])
        d <- unlist(result[2])
        mrecs <- data.frame(as.integer(m))
        mrecs$d <- d
        names(mrecs) <- c("movie_id", "distance")
        mrecs$title <- lookup(mrecs$movie_id, movies[,c(1:2)])
        names(mrecs)[3] <- "title"
        mrecs <- arrange(mrecs, distance)
        mrecs
}

characterizeMoviesByCluster <- function(mu, k) {
        ## Returns the k movies with the largest dot product to mu
        mu <- matrix(mu, nrow=1, ncol=1)
        mu_similarity <- numeric(N2)
        for (j in 1:nrow(V)) {
                mu_similarity[j] <- sum(mu * V[j,])
        }
        kNC <- rank(mu_similarity, ties.method="random")
        top_kNC <- kNC[1:k]
        mcluster <- data.frame(as.integer(top_kNC))
        names(mcluster)[1] <- "movie_id"
        mcluster$title <- lookup(mcluster$movie_id, movies[,c(1:2)])
        return(mcluster)
}

genDataFromGaussMixture <- function(d, N, MU, SIGMA, PI) {
        ## Generate N observations in R^d from a pre-defined mixture of Gaussians
        data <- matrix(rep(0), nrow=N, ncol=d)
        g  <- integer(N)
        sd <- as.vector( sqrt(diag(SIGMA)) )
        Fx <- cumsum(PI)
        U  <- runif(N)
        for (i in 1:N) {
                g[i] <- min( which(Fx >= U[i]) )
                mu <- unlist(MU[g])
                data[i,] <- rnorm( d, mu, sd )
        }
        return(list(data, g))
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
