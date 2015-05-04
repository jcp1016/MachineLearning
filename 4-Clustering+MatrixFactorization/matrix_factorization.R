##
## Machine Learning - HW4 Part 2
## Implement probabalistic matrix factorization on a dataset of user_ids, movie_ids, and ratings from 1 to 5.
##
require("dplyr")
require("foreach")
require("parallel")
require("foreach")
require("doMC")
require("ggplot2")
require("ggthemes")
require("qdapTools")
numCores <- detectCores()
registerDoMC(numCores)

source("functions.R")

lambda <- 10
sigma2 <- 0.25
c1 <- 1 / (2 * sigma2)
c2 <- lambda/2
d <- 20
T <- 100

## Read input files
ratings <- read.csv("./movies_csv/ratings.txt", stringsAsFactors=FALSE, check.names=FALSE)
ratings_test <- read.csv("./movies_csv/ratings_test.txt", stringsAsFactors=FALSE, check.names=FALSE)
names(ratings) <- c("user_id", "movie_id", "rating")
names(ratings_test) <- c("user_id", "movie_id", "rating")
movies <- read.csv("./movies_csv/movies.txt", stringsAsFactors=FALSE, header=FALSE, sep="@")
movies <- cbind(c(1:nrow(movies)), movies)
names(movies) <- c("movie_id", "movie_title")

users <- unique(ratings$user_id)

## Build omega_u and omega_v indexes
N1 <- length(users)
N2 <- nrow(movies)
OMEGA_u <- as.data.frame(matrix(rep(0), nrow=N1, ncol=N2))
OMEGA_v <- as.data.frame(matrix(rep(0), nrow=N2, ncol=N1))

sratings <- arrange(ratings, user_id, movie_id)
for (i in 1:N1) {
        objects_rated <- filter(sratings, user_id == i)[2]
        for (j in objects_rated)
                OMEGA_u[i,j] <- 1
}
sratings <- arrange(ratings, movie_id, user_id)
for (j in 1:N2) {
        users_who_rated <- filter(sratings, movie_id == j)[1]
        for (i in users_who_rated)
                OMEGA_v[j,i] <- 1
}

## Define U and V matrices, and initialize V
U <- matrix(rep(0), nrow=N1, ncol=d, byrow=TRUE)
v_sd <- rep(1/lambda, d) ## vector of standard deviations for v
set.seed(25)
V <- rnorm(N2*d, 0, v_sd)
V <- matrix(V, nrow=N2, ncol=d, byrow=TRUE)
I <- diag(d)  ## generates a dxd identity matrix
p1  <- (lambda*sigma2*I) # 20x20

## Train the model for 100 iterations.
## L is the objective function.
## U and V will be trained by coordinate ascent as d-dimensional location vectors.
T <- 100
setwd("./output")
L <- numeric(T)
for (t in 21:40) {
        L[t] <- 0
        L_ti <<- 0
        system.time({
        result <- foreach(i = 1:N1) %dopar% {
                calculateU(i)
        }
        })
        U <- matrix( unlist(result[1:N1]), nrow=N1, ncol=d, byrow=TRUE)
        fn <- paste0("U_", t)
        write.table(U, fn)

        L[t] <- -c1 * L_ti - c2 * sum(U^2) - c2 * sum(V^2)
        cat("L",t,"=", L[t], " ")

        system.time({
        result <- foreach(j = 1:N2) %dopar% {
                calculateV(j)
        }
        })
        V <- matrix( unlist(result[1:N2]), nrow=N2, ncol=d, byrow=TRUE)
        fn <- paste0("V_", t)
        write.table(V, fn)
}

## Make predictions on the test set and compute the RMSE at each training iteration
n <- nrow(ratings_test)
RMSE <- numeric(T)
setwd("./output")
for (t in 1:T) {
        ufn <- paste0("U_",t)
        vfn <- paste0("V_",t)
        U <- as.matrix(read.table(ufn))
        V <- as.matrix(read.table(vfn))
        p <- foreach(i = 1:n) %dopar% {
                u <- ratings_test$user_id[i]
                m <- ratings_test$movie_id[i]
                p1 <- t(U[u,]) %*% V[m,]
                p1 <- round(p1,0)
                if (p1 < 1) 1
                else if (p1 > 5) 5
                else p1
        }
        pred <- unlist(p)
        ratings_test$pred <- pred
        ratings_test$pred_error <- ratings_test$rating - ratings_test$pred
        RMSE[t] <- sqrt(mean(ratings_test$pred_error^2))
}

## Plot the RMSE and objective function for the test set at each training iteration
ggplot(data=as.data.frame(RMSE), aes(x=c(1:T), y=RMSE, color="")) +
        geom_point(shape=19, position="identity") +
        geom_line() +
        theme_bw() +
        theme(legend.title=element_blank()) +
        theme(legend.position="none") +
        scale_fill_hue() +
        xlab("iteration") +
        scale_x_discrete() +
        scale_y_continuous() +
        ggtitle("RMSE at each iteration")
ggsave(filename="RMSE.png", width=19.52, height=15.5)

ggplot(data=as.data.frame(L), aes(x=c(1:T), y=L, color="")) +
        geom_point(shape=19, position="identity") +
        geom_line() +
        theme_bw() +
        theme(legend.title=element_blank()) +
        theme(legend.position="none") +
        scale_fill_hue() +
        xlab("iteration") +
        scale_x_discrete() +
        scale_y_continuous() +
        ggtitle("Log joint likelihood at each iteration")
ggsave(filename="L.png", width=19.52, height=15.5)

## Pick several movies and find the 5 nearest movies based on Euclidean distance, where
## V contains a 20 dimensional location vector for each film.
U <- as.matrix(read.table("./output/U_100"))
V <- as.matrix(read.table("./output/V_100"))

k <- 5
for (movie_id in c(70, 196, 496, 1, 26, 134)) {
        result <- recommendMoviesByDistance(movie_id, k)
        print(result)
}

## Perform K-means on U with K=30
K <- 30
T <- 20
X <- U
N <- nrow(X)
MU <- matrix(rep(0), nrow=K, ncol=T)
C  <- matrix(rep(0), nrow=N, ncol=T)
mu <- sample(X, K) ## initialize MU with K random points from X
MU[,1] <- mu
for (t in 1:T) {
        c <- assignToCluster(X, MU[,t], K)
        c <- rapply( list(c), f=function(x) ifelse(is.nan(x), 0, x), how="replace" )
        C[,t] <- unlist(c)
        if (t < T) {
                mu <- updateCentroids(X, C[,t], K, t)
                mu <- rapply( list(mu), f=function(x) ifelse(is.nan(x), 0, x), how="replace" )
                MU[,t+1] <- unlist(mu)
        }
}
write.table(MU,file="saved_MU")

## For each non-zero centroids, take the dot product with all movies, and show the k movies whose dot product is largest.
k <- 10
for (mu in c(1, 2, 3, 4, 10, 25, 29, 30)) {
        mu <- MU[i,T]
        result <- characterizeMoviesByCluster(mu, k)
        print(result)
}

k <- 10
for (i in 1:nrow(MU)) {
        mu <- MU[i,8]
        result <- characterizeMoviesByCluster(mu, k)
        print(result)
}
