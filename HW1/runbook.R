## Read the data
setwd("~/Columbia/MachineLearning/HW1/data_csv-2")
y <- read.csv("y.txt", header = FALSE)
names(y) <- "mpg"

X <- read.csv("X.txt", header=FALSE)
names(X) <- c("intercept_term",
              "number_of_cylinders",
              "displacement",
              "horsepower",
              "weight",
              "acceleration",
              "model_year")


setwd("~/Columbia/MachineLearning/HW1")
alldata <- cbind(y,X)

## Randomly split data into a training set of 372 obs and a test set of 20 obs
train <- sample(seq_len(nrow(alldata)), size = 372)
trainingset <- alldata[train,]
testset     <- alldata[-train,]
dim(trainingset)
dim(testset)
Ytrain <- as.matrix(trainingset[,1])
Xtrain <- as.matrix(trainingset[,2:8])
Ytest  <- as.matrix(testset[,1])
Xtest  <- as.matrix(testset[2:8])

source("functions.R")
plot_data(trainingset)


w <- fit_wml(Xtrain, Ytrain)
print("(1a)", quote=FALSE)
print(w)
