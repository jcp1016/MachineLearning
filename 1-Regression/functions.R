## Compute least squares regression coefficients using matrices
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

## Use model to predict Y values for a test set
predict_Y <- function(X = Xtest, w = wml) {
  Ypred <- X %*% w
  Ypred
}

## For t experiments, compute Mean Absolute Error (MAE) and Root Mean-Squared Error (RMSE)
repeat_process <- function(t = 1000, ntrain = 372, data = alldata, p = 1, showplot = "Y") {
        MAE   <- as.numeric(rep(NA, t))
        RMSE  <- as.numeric(rep(NA, t))
        ntest <- nrow(alldata) - ntrain
        pred_error <- matrix(numeric(0), nrow = ntest, ncol = t)

        for (i in 1:t) {
                ## Randomly split data into a training set and a test set
                train       <- sample(seq_len(nrow(data)), size = ntrain)
                trainingset <- data[train,]
                testset     <- data[-train,]

                ## Prep for least squares regression model fitting
                d <- ncol(data)
                Ytrain      <- as.matrix(trainingset[,1])
                Xtrain      <- as.matrix(trainingset[,2:d])
                Ytest       <- as.matrix(testset[,1])
                Xtest       <- as.matrix(testset[2:d])
                names(Ytrain) <- "mpg"

                ## Solve for vector of least squares regression coefficients
                wml <- fit_wml(Xtrain, Ytrain, t)

                ## Predict Y for the test samples
                Ypred <- predict_Y(Xtest, wml)

                n <- nrow(Ytest)
                if (n > 0) {
                        ## Calculate the mean absolute error (MAE) and root mean squared error (RMSE)
                        ## of the predictions.
                        MAE[i] <- 0
                        RMSE[i] <- 0
                        for (j in 1:n) {
                                pred_error[j,i] <- Ytest[j] - Ypred[j]
                                MAE[i]  <- MAE[i]  + abs(pred_error[j,i])
                                RMSE[i] <- RMSE[i] + (pred_error[j,i]^2)
                        }
                        MAE[i]  <- MAE[i] / n
                        RMSE[i] <- sqrt(RMSE[i] / n)
                }

        }
        if (t == 1) {  ## print results for part 3.1.a
                wml <- data.frame(wml)
                names(wml)[1] <- "Coefficient"
                return(wml)
                ## to verify, compare with results from R lm function
                ## df <- cbind(Ytrain, Xtrain)
                ## lm_model <- lm(df[,1] ~ df[,3] + df[,4] + df[,5] + df[,6] + df[,7] + df[,8])
                ## cat("\n Compare with results from lm: \n",  t(coef(lm_model)), "\n")
        } else {
                RMSE <- as.matrix(RMSE)
                mean_RMSE   <- calc_MLE_mean(RMSE)
                sd_RMSE     <- calc_MLE_sd(RMSE, mean_RMSE)
                mean_error  <- calc_MLE_mean(pred_error)
                sd_error    <- calc_MLE_sd(pred_error, mean_error)
                results <- data.frame(p, mean_RMSE, sd_RMSE, mean_error, sd_error)
                names(results) <- c("p", "MeanRMSE", "StdDevRMSE", "MeanPredError", "StdDevPredError")
                if (showplot == "Y") {
                        hist(pred_error,
                             main = paste("p = ", p),
                             col="gray",
                             xlab = "Ytest - Ypred")
                }
                return(results)
        }
}

calc_MLE_mean <- function(X) {
        n  <- nrow(X) * ncol(X)
        mu <- 0
        if (n > 0) {
                mu <- sum(X) / n
        }
        mu
}

calc_MLE_sd <- function(X, mu) {
        r <- nrow(X)
        c <- ncol(X)
        n <- r * c

        sigma <- 0
        sigma_ml <- 0
        if (n > 0) {
                for (i in 1:r) {
                        for (j in 1:c) {
                                sigma <- sigma + (X[i,j] - mu)^2
                        }
                }
                sigma_ml <- sqrt(sigma / n)
        }
        sigma_ml
}

## Make some exploratory plots, although not required for the HW
plot_data <- function(df) {

        layout(matrix(c(1,1,1,2,3,4,5,6,7), 3, 3, byrow = TRUE), respect = TRUE)

        hist(t(y), main = "Distribution of Miles per Gallon", col="gray", xlab = "MPG")

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

        layout(1,1)
}

## Green for positive slope, red for negative slope
get_color <- function(lm_model) {

        if (coefficients(lm_model)[2] >= 0) "darkseagreen" else "indianred3"

}


