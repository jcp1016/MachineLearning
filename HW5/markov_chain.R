require("data.table")
require("ggplot2")
require("ggthemes")

scores <- read.csv("./hw5text/cfb2014scores.csv", header=FALSE)
legend <- read.table("./hw5text/legend.txt", header=FALSE, stringsAsFactors=FALSE)
names(scores) <- c("team1_index", "team1_points", "team2_index", "team2_points")

N <- nrow(legend)
M <- Mhat <- matrix(rep(0), nrow=N, ncol=N)
for (i in 1:nrow(scores)) {
        ind1 <- ind2 <- 0
        j1   <- scores[i,1]
        pts1 <- scores[i,2]
        j2   <- scores[i,3]
        pts2 <- scores[i,4]
        total_pts <- sum(pts1, pts2)
        if (pts1 > pts2) {
                ind1 <- 1 
        } else { 
                ind2 <- 1
        }
        Mhat[j1,j1] <- Mhat[j1,j1] + ind1 + pts1/total_pts    
        Mhat[j2,j2] <- Mhat[j2,j2] + ind2 + pts2/total_pts
        Mhat[j1,j2] <- Mhat[j1,j2] + ind2 + pts2/total_pts
        Mhat[j2,j1] <- Mhat[j2,j1] + ind1 + pts1/total_pts
}
total <- as.vector(rowSums(Mhat))
M <- Mhat / total

## List the top 20 teams and their corresponding values in w_t for t = {10, 100, 200, 1000}
T <- 1000
w <- matrix(rep(0), nrow=T, ncol=N)
w[1,] <- rep(1/N, N)   ## initialize w with a uniform distribution
for (t in 2:T) {
       w[t,] <- w[t-1,] %*% M  
}
wT <- data.table(matrix(w[T,], nrow=N, ncol=1))
setnames(wT, "MC_score")
wT$team <- legend$V1
rankings <- wT[order(-wT$MC_score),]
print(rankings[1:20,])
##--------------------------------------------------------------------------
## Find the first eigenvector of t(M), i.e. where the eigenvalue = 1
## Note:  The eigen function returned two eigenvectors with eigenvalue=1.  
##        I tried both and chose the one closer to w_t in L1 distance.
##--------------------------------------------------------------------------
own <- eigen(t(M), symmetric=FALSE)
u1  <- as.numeric(own$vectors[,2]) 

## Plot the L1 norm of w[t,] - w_inf, for t=1,...,1000
w_inf <- u1 / sum(u1)
dist <- numeric(T)
for (t in 1:T) {
        dist[t] <- sum( abs(w[t,] - w_inf) )  
}
dist <- as.data.frame(dist)
names(dist)[1] <- "distance"
dist$t <- c(1:T)
ggplot(data = dist, aes(x=t, y=distance, color="")) +
        geom_point() +
        theme_bw() + 
        scale_fill_hue() + 
        xlab("iteration") + 
        ylab("L1 distance") +
        theme(legend.position="none") +
        theme(legend.title=element_blank()) +
        ggtitle(paste0("L1 distance between w_t and w_infinity at each iteration"))
ggsave(file="p1_3.png")

print(dist[T,1])
## 0.02992594

rm(dist)
