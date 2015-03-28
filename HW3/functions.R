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

classifyBayesLinear <- function(Bt) {
        d <- ncol(Bt) - 1
        classifier <- integer(d)
        
        MU0 <- getMu(-1, Bt)
        MU1 <- getMu(1, Bt)
        
        PI0 <- getPrior(-1, Bt)
        PI1 <- getPrior(1, Bt)
        
        MU    <- getMu("Combined", Bt)
        SIGMA <- getSigma(Bt[,3:d], t(MU)) 
        SIGMA_I <- solve(SIGMA)

        X  <- as.matrix(X)
        y  <- as.integer(y)
        n  <- nrow(Ytrain)
        d  <- ncol(X)
        w0 <- w <- 0
        p1 <- p2 <- p3 <- p4 <- 0
                
        p1 <- -0.5 * t(X - class_MU)
        p2 <- p1 %*% class_SIGMA_I
        p3 <- p2 %*% (x - class_MU)
        p4 <- exp(p3)
        classifier[k] <- prod(class_prior, 1/sqrt(det(class_SIGMA)), p4)

        winner <- as.integer( which (classifier == max(classifier)) )
        if ((winner-1) != y) {
                cat("\n", case, y, winner-1)
        }
        as.integer( winner-1 )
}

getMu <- function(class, sample) {
        if (class != "All") {
                g1 <- filter( sample, y==class )
                MU <- as.matrix( summarise_each(g1, funs(mean)) )
        } else {
                g1 <- sample
                MU <- as.matrix( summarise(g1, funs(mean)))
        }
        MU[1,-1]
}

getSigma <- function(MU, X) {
        ## Assumes X has a bias term in the first column (i.e. a column of 1's) 
        d <- ncol(X) - 1  
        for ( j in 1:d ) {
                x[,j] <- x[,j] - mu
        }
        SIGMA <- as.matrix( t(x) %*% x )
        SIGMA <- SIGMA / class_n
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
