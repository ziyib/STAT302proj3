#' Linear Regression function
#'
#' This function fits a linear model in R
#'
#' @param formula a formula class object, similar to lm().
#' @param data input data frame.
#'
#' @keywords \code{inference}
#'
#' @return table similar to the coefficent table from summary() with rows for each coefficient
#' and columns for the Estimate, Std. Error, t value, and Pr(>|t|).
#'
#' @examples
#' my_lm (mpg ~ hp + wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data) {
        #Create a model matrix and response through helper functions
        x <- model.matrix(formula, data)
        y <- model.response(model.frame(formula, data))
        beta <- solve(t(x) %*% x) %*% t(x) %*% y
        #Calculate the standard error using df and variance
        df <- nrow(x) - ncol(x)
        variance <- sum((y - x %*% beta)^2 / df)
        se <- sqrt(diag(variance * solve(t(x) %*% x)))
        #Calculate the p value and t value using pt
        test_stats <- (beta - 0) / se
        p_value <- 2 * pt(abs(test_stats), df, lower.tail = FALSE)
        t_value <- beta / se
        #define the output as similar to the summary of lm
        #including estimate, standard error, t value and p-value
        output <- list("Estimate" = beta,
                       "Std. Error" = se,
                       "t value" = t_value,
                       "Pr(>|t|)" = p_value)
        return(output)
}
