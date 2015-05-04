require("ggplot2")
require("ggthemes")

source("functions.R")
## ------------------------------------------------------------------------------------------
## Implement non-negative matrix factorization (NMF) using the divergence penalty;
## Each row in dataset nyt_data.txt corresponds to a single document.  It contains the index 
## of the words appearing in the document and the frequency.  It uses "idx:cnt" format 
## with commas separating each unique word in the document.  
## ------------------------------------------------------------------------------------------
D <- read.csv("./hw5text/nyt_data.txt", header=FALSE, stringsAsFactors=FALSE)
V <- read.table("./hw5text/nytvocab.dat", header=FALSE, stringsAsFactors=FALSE)
k <- 25
T <- 200
X <- constructTermFreqMatrix(D, V)
write.table(X, file="X_dp_saved")

result <- runNMF_DP(X, k, T)
L <- unlist(result[1])
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
        ylab("Divergence objective") +
        theme(legend.position="none") +
        theme(legend.title=element_blank()) +
        ggtitle(paste0("Objective function at each iteration"))
ggsave(file="p2_2.png")

## Normalize the columns of W so they sum to 1
total <- as.vector(colSums(W))
Wnorm <- W / total

## Pick 5 columns of Wnorm.  For each selected column show the 10 words having the largest probability 
## according to the values in W, and give the probabilities.  The ith row of W corresponds with the ith
## word in the dictionary provided with the data.
for (mycol in c(4, 11, 15, 17, 24)) {
        rowNum <- order(Wnorm[,mycol], decreasing=TRUE)[1:10]
        words <- V[rowNum,]
        probabilties <- Wnorm[rowNum[1:10], mycol]
        cat("\n\nColumn number: ", mycol)
        for (i in 1:10) {
                cat("\n", words[i], "\t", Wnorm[rowNum[i],mycol])
        }
}

