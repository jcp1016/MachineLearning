require("ggplot2")
require("ggthemes")
source("km_functions.R")

## Generate 500 observations from a mixture of three Gaussians on R2 with PI = [0.2, 0.5, 0.3]
N  <- 500
MU <- list( c(0,0), c(3,0), c(0,3) ) 
SIGMA <- matrix( c(1,0,0,1), nrow=2, byrow=TRUE )
PI <- c(0.2, 0.5, 0.3)
results <- genDataFromGaussMixture(2, N, MU, SIGMA, PI)
X <- unlist( results[1] )
X <- matrix(X, nrow=N, byrow=TRUE)
g <- unlist( results[2] )
ggplot(as.data.frame(g), aes(x=g)) +
        geom_histogram(binwidth=1, color="black", fill="white") +
        scale_x_discrete()
rm(MU)
rm(SIGMA)

## Run k-means 20 times for each choice of K
K <- 5
T <- 20
N <- nrow(X)
MU <- matrix(rep(0), nrow=T, ncol=K)
C  <- matrix(rep(0), nrow=T, ncol=N)
L1 <- L2 <- numeric(T)
MU[1,] <- sample(X, K, replace=FALSE)  ## initialize MU with K random points from X
for (t in 1:T) {
        C[t,] <- assignToCluster(X, MU[t,], K) 
        L1[t] <- calcObjFunction(X, MU[t,], C[t,], K)
        if (t < T) {
                MU[t+1,] <- updateCentroids(X, C[t,], K, t)
                L2[t]    <- calcObjFunction(X, MU[t+1,], C[t,], K)
        } else {
                L2[t] <- L1[t]
        }   
}

## Plot the objective function
pdata <- as.data.frame(L1)
pdata$L2 <- L2
pdata$t <- c(1:T)
pdata$tm <- pdata$t - 0.5
ggplot(pdata) +
        geom_point(shape=19, position="identity", alpha=1, cex=2.5,
                   aes(x=pdata$tm, y=pdata$L1, colour="Assignment")) +
        geom_point(shape=19, position="identity", alpha=1, cex=2.5,
                   aes(x=pdata$t, y=pdata$L2, colour="Update")) +
        theme_bw() + scale_fill_hue() + xlab("iteration") + ylab("") +
        scale_x_discrete() + 
        theme(legend.title=element_blank()) + 
        ggtitle(paste0("Objective function at each iteration\nK = ",K))
fn <- paste0("L_K",K,".png")
ggsave(filename=fn)

## Plot the data set, indicating the cluster of each point
pdata <- as.data.frame(X)
pdata$Cluster <- as.character(C[T,])
ggplot(pdata, aes(x=V1, y=V2, color = Cluster)) +
        geom_point() +
        theme_bw() + scale_fill_hue() +
        scale_x_discrete() +
        scale_y_discrete() +
        ggtitle(paste0("K = ",K))
fn <- paste0("Data_K",K,".png")
ggsave(filename=fn)
