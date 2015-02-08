## R script for questions 3.1a and 3.1b
## Functions fit_wml and predict_Y are in functions.R
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

repeat_tests(1, 372, alldata)
repeat_tests(1000, 372, alldata)

