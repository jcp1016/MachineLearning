## For questions 3.1.a and 3.1.b
## Compute least squares regression using matrices
fit_wml <- function(X = Xtrain, y = Ytrain, mode = 1) {

        X <- as.matrix(X)
        Y <- as.matrix(y)

        ## t() is an R fn that returns the transpose of a matrix
        Xt <- t(X)  

        ## solve() is an R fn that returns the inverse of a matrix
        XtXinv_Xt <- solve(Xt %*% X) %*% Xt  
        
        ## Project onto Y to get the coefficients
        wml <- XtXinv_Xt %*% Y       
        wml
}

## For question 3.1.b, predict Y values for a test set
predict_Y <- function(X = Xtest, w = wml) {
  Ypred <- X %*% w
  Ypred 
}

## Run t number of tests and compute Mean Absolute Error (MAE)
repeat_tests <- function(t = 1000, ntrain = 372, data = alldata) {
        MAE <- as.numeric(rep(NA, t))
        for (i in 1:t) {
                ## Randomly split data into a training set and a test set
                train       <- sample(seq_len(nrow(data)), size = ntrain)
                trainingset <- data[train,]
                testset     <- data[-train,]
                
                ## Prep for least squares regression model fitting
                Ytrain      <- as.matrix(trainingset[,1])
                Xtrain      <- as.matrix(trainingset[,2:8])
                Ytest       <- as.matrix(testset[,1])
                Xtest       <- as.matrix(testset[2:8])
                names(Ytrain) <- "mpg"
        
                ## Solve for vector of least squares regression coefficients
                wml <- fit_wml(Xtrain, Ytrain, t)  
        
                ## Predict Y for the test samples
                Ypred <- predict_Y(Xtest, wml) 
        
                ## Calculate the Mean Absolute Error (MAE) of the predictions
                n <- nrow(Ytest)
                if (n > 0) {
                        MAE[i] <- 0
                        for (j in 1:n) {
                                MAE[i] <- MAE[i] + abs(Ytest[j] - Ypred[j])
                        }
                        MAE[i] <- MAE[i] / n
                }
        }
        if (t == 1) {  ## print results for part 3.1.a
                cat(" wml = \n")
                print(wml)
                ## to verify, compare with results from R lm function
                ##df <- cbind(y, X)
                ##lm_model <- lm(df[,1] ~ df[,3] + df[,4] + df[,5] + df[,6] + df[,7] + df[,8])
                ##cat("\n Compare with results from lm: \n",  t(coef(lm_model)), "\n")
        } else {  ## print results for part 3.1.b
                cat(" Number of tests = ", t, "\n")
                cat(" MAE Mean = ", mean(MAE, na.rm = TRUE), "\n")
                cat(" MAE Standard Deviation = ", sd(MAE, na.rm = TRUE), "\n")
        }
}

## Make some exploratory plots
plot_data <- function(df) {
        
        layout(matrix(c(1,1,1,2,3,4,5,6,7), 3, 3, byrow = TRUE), respect = TRUE)
        
        hist(t(y), main = "Distribution of Miles per Gallon", col="gray", xlab = "MPG")
        #dmpg <- density(df$mpg)
        #plot(dmpg, main = "Distribution of Miles per Gallon")
        
        model <- lm(df$mpg ~ df$number_of_cylinders)
        with(df, plot(number_of_cylinders, mpg, main = "MPG and Cylinders(Std)",
                      col  = get_color(model),
                      xlab = "", ylab = "MPG"))
        abline(model, lwd=2)
        
        model <- lm(df$mpg ~ df$displacement)
        with(df, plot(displacement, mpg, main = "MPG and Displacement(Std)",
                      col  = get_color(model),
                      xlab = "", ylab = "MPG"))
        abline(model, lwd=2)
        
        model <- lm(df$mpg ~ df$horsepower)
        with(df, plot(horsepower, mpg, main = "MPG and Horsepower(Std)",
                      col  = get_color(model),
                      xlab = "", ylab = "MPG"))
        abline(model, lwd=2)
        
        model <- lm(df$mpg ~ df$weight)
        with(df, plot(weight, mpg, main = "MPG and Weight(Std)",
                      col  = get_color(model),
                      xlab = "", ylab = "MPG"))
        abline(model, lwd=2)
        
        model <- lm(df$mpg ~ df$acceleration)
        with(df, plot(acceleration, mpg, main = "MPG and Acceleration(Std)",
                      col  = get_color(model),
                      xlab = "", ylab = "MPG"))
        abline(model, lwd=2)
        
        model <- lm(df$mpg ~ df$model_year)
        with(df, plot(model_year, mpg, main = "MPG and Model Year(Std)",
                      col  = get_color(model),
                      xlab="", ylab = "MPG"))
        abline(model, lwd=2)
}

## Function to set color
get_color <- function(lm_model) {
        
        if (coefficients(lm_model)[2] >= 0) "darkseagreen" else "indianred3"
        
}


