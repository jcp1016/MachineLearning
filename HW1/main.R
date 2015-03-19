## R script for HW 1
## All functions are in functions.R
##
## Assumptions:
##    1)  X.txt contains 392 observations of 7 variables
##    2)  y.txt contains 392 observations of 1 variable (mpg)
##    3)  There are no missing values
##    4)  *.txt files are in a subdirectory named data_csv-2
##

## Read the data
setwd("./data_csv-2")
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
alldata <- cbind(y,X)
setwd("../")
source("functions.R")
#plot_data(trainingset)

## Split data into training and test sets, solve a linear regression model 
## for the training set using least squares, and print the vector wml.
repeat_process(t = 1, ntrain = 372, data = alldata, showplot = "N")

## Repeat the process 1000 times, calculate the mean absolute error (MAE) of the 
## predictions, and print the mean and standard deviation of the MAE.
repeat_process(t = 1000, ntrain = 372, data = alldata, showplot = "N")

## Fit a p order polynomial regression model using least squares 
## for p = 1, 2, 3, 4.  Print the mean and standard deviation of the RMSE and plot 
## a histogram of the prediction errors for each value of p.
plot.new()
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), respect = TRUE)

rmse_by_p <- data.frame(matrix(numeric(0), nrow = 4, ncol = 5))
names(rmse_by_p) <- c("p", "Mean RMSE", "Std Dev RMSE", "Mean Pred Error", "Std Dev Pred Error")

rmse_by_p[1,] <- repeat_process(t = 1000, ntrain = 372, data = alldata) 
## TO DO:  calculate log-likelihood for each p  *******

X2 <- (X[,2:7])^2
alldata_2 <- cbind(alldata, X2)
rmse_by_p[2,] <- repeat_process(t = 1000, ntrain = 372, data = alldata_2, p = 2)

X3 <- (X[,2:7])^3
alldata_3 <- cbind(alldata_2, X3)
rmse_by_p[3,] <- repeat_process(t = 1000, ntrain = 372, data = alldata_3, p = 3)

X4 <- (X[,2:7])^4
alldata_4 <- cbind(alldata_3, X4)
rmse_by_p[4,] <- repeat_process(t = 1000, ntrain = 372, data = alldata_4, p = 4)

cat("\n")
print(rmse_by_p, row.names = FALSE)
layout(1,1)
