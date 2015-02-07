## Function to set color
get_color <- function(lm_model) {

        if (coefficients(lm_model)[2] >= 0) "darkseagreen" else "indianred3"

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

## Compute least squares regression using matrices
fit_wml <- function(X,y) {

        X <- as.matrix(X)
        Y <- as.matrix(y)

        Xt <- t(X)  ## t returns the transpose of a matrix

        XtXinv_Xt <- solve(Xt %*% X) %*% Xt  ## solve returns the inverse of a matrix

        ## Project onto Y to get the coefficients
        wml <- XtXinv_Xt %*% Y
        wml

        ## Compare this with lm output
        ## df <- cbind(y,X)
        ## lm(df$mpg ~ df$cylinders + df$displacement + df$weight +
        ##      df$hp + df$model_year + df$acceleration)

}
