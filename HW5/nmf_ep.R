require("ggplot2")
require("ggthemes")
require("graphics")

source("functions.R")
## -------------------------------------------------------------------------------
## Implement non-negative matrix factorization (NMF) using the Euclidean penalty;
## The dataset faces.csv contains 1000 facial images, represented as columns.  
## Each image is 32x32 but has been vectorized to 1024 numbers.
## Number of dimensions=1024, Number of objects=1000.
## -------------------------------------------------------------------------------
X <- read.csv("./hw5text/faces.csv", header=FALSE, stringsAsFactors=FALSE)
k <- 25
T <- 200
result <- runNMF_EP(X, k, T)
L <- unlist(result[1])  ## there is a problem here.  W is not plotting correctly when returned from fn 
W <- matrix(unlist(result[2]), nrow=nrow(X), ncol=k, byrow=TRUE)
H <- matrix(unlist(result[3]), nrow=k, ncol=ncol(X), byrow=TRUE)
rm(result)

## Plot the objective function over the iterations
L <- as.data.frame(L)
names(L)[1] <- "L"
L$t <- c(1:T)
ggplot(data = L[-1,], aes(x=t, y=L, color="")) +
        geom_point() +
        theme_bw() + 
        scale_fill_hue() + 
        xlab("iteration") + 
        ylab("Squared error objective") +
        theme(legend.position="none") +
        theme(legend.title=element_blank()) +
        ggtitle(paste0("Objective function at each iteration"))
ggsave(file="p2_1.png")

##-----------------------------------------------------------
## Show the original image and the column of W for which the 
## corresponding weight in H is largest
##-----------------------------------------------------------
#layout(matrix(c(3:2), 3, 2, byrow = TRUE), respect = TRUE)
#par(mfrow=c(3,2))
layout(matrix(c(1:2), 1, 2, byrow = TRUE), respect = TRUE)
par(mfrow=c(1,2))

for (img in c(15,500,850)) {
        MX <- matrix(X[,img], nrow=32, ncol=32, byrow=TRUE)
        image(rotateMatrix(rotateMatrix(MX)), col=gray((0:50)/50))
        
        Hwt <- which.max(H[,img])
        cat("\nHwt=", Hwt)

        MW <- matrix(W[,Hwt], nrow=32, ncol=32, byrow=TRUE)
        image(rotateMatrix(rotateMatrix(MW)), col=gray((0:50)/50))
}