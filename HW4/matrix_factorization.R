require("dplyr")
#source("mf_functions.R")

lambda <- 10
sigma2 <- 0.25
d <- 20
T <- 100

setwd("./movies_csv")
ratings <- read.csv("ratings.txt", stringsAsFactors=FALSE, check.names=FALSE)
ratings_test <- read.csv("ratings_test.txt", stringsAsFactors=FALSE, check.names=FALSE)
names(ratings) <- c("user_id", "movie_id", "rating")
names(ratings_test) <- c("user_id", "movie_id", "rating")
movies <- read.csv("movies.txt", stringsAsFactors=FALSE, header=FALSE, sep="@")
movies <- cbind(c(1:nrow(movies)), movies)
names(movies) <- c("movie_id", "movie_title")
users <- unique(ratings$user_id)

N1 <- length(users)
N2 <- nrow(movies)
OMEGA_u <- as.data.frame(matrix(rep(FALSE), nrow=N1, ncol=N2))
OMEGA_v <- as.data.frame(matrix(rep(FALSE), nrow=N2, ncol=N1))

sratings <- arrange(ratings, user_id, movie_id)
for (i in 1:N1) {
        objects_rated <- filter(sratings, user_id == i)[2]
        for (j in objects_rated) 
                OMEGA_u[i,j] <- TRUE
}
sratings <- arrange(ratings, movie_id, user_id)
for (j in 1:N2) {
        users_who_rated <- filter(sratings, movie_id == j)[1]
        for (i in users_who_rated)
                OMEGA_v[j,i] <- TRUE
}

U <- matrix(rep(0), nrow=N1, ncol=d, byrow=TRUE)
v_sd <- rep(1/lambda, d) ## vector of standard deviations for v
V <- rnorm(N2*d, 0, v_sd)
V <- matrix(V, nrow=N2, ncol=d, byrow=TRUE)
I <- diag(d)  ## generates a dxd identity matrix  
p1  <- (lambda*sigma2*I) # 20x20

for (t in 1:T) {
        for (i in 1:N1) {
                ind <- which( OMEGA_u[i,] == TRUE )
                p2 <- t(V[ind,]) %*% V[ind,] # 20x20
                p3 <- solve(p1 + p2) #20x20
                M <- matrix(rep(0), nrow=1, ncol=N2)
                for (j in ind) {
                        m1 <- filter(ratings, user_id==i, movie_id==j)
                        M[1,j] <- as.integer( m1$rating )
                }
                p4 <- M %*% V
                U[i,] <- p4 %*% p3 ## 1xd
        }
        for (j in 1:N2) {
                ind <- which( OMEGA_v[j,] == TRUE )
                p2 <- matrix(rep(0), nrow=d, ncol=d)
                if (is.matrix(U[ind,])) {
                        p2 <- t(U[ind,]) %*% U[ind,]
                } else {
                        p2 <- U[ind,] %*% t(U[ind,])
                }
                p3 <- solve(p1 + p2) #solve finds the inverse of a square matrix
                M <- matrix(rep(0), nrow=1, ncol=N1)
                for (i in ind) {
                        m1 <- filter(ratings, user_id==i, movie_id==j)
                        M[1,i] <- as.integer( m1$rating )
                }
                p4 <- M %*% U
                V[j,] <- p4 %*% p3 ## 1xd
        }
}