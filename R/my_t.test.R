#' t-test function
#'
#' This function performs a one sample t-test in R.
#'
#' @param x a numeric vector of data.
#' @param alternative a character string specifying the alternative hypothesis.
#' This should only accept "two.sided", "less", or "greater".
#' Otherwise, the function should throw an informative error asking you to try again.
#' @param  mu a number indicating the null hypothesis value of the mean.
#'
#' @import dplyr class tidyr
#'
#' @keywords inference
#'
#' @return test_stat represents the numeric test statistic, df represents the degrees of freedom.
#' alternative represents the value of the parameter alternative, and p_val represents  the numeric p-value.
#'
#' @examples
#' my_t_test(x = rnorm(100), alternative = "less", mu = 0 )
#' my_t_test(x = rnorm(100), alternative = "greater", mu = 0 )
#'
#' @export
my_t_test <- function(x,alternative,mu){
        ##Define the standard error  test statistics, degree of freedom and which sided should be used
        se <- sd(x)/sqrt(length(x))
        test_stat <- (mean(x) - mu)/se
        df <- length(x) - 1
        alternative_response <- c("two.sided", "less", "greater")
        ##Use if else statement to filter the condition under which we calculate the p value
        if (alternative == "two sided") {
                p_val <- 2* pt(abs(test_stat), df, lower.tail=FALSE)
        } else if (alternative == "greater") {
                p_val <- pt(test_stat, df, lower.tail = TRUE)
        } else if (alternative == "less") {
                p_val <- pt(test_stat, df, lower.tail = FALSE)
        } else {
                stop("please try again")
        }
        ##Define the output as the test statistics, degree of freedom, p value and the type of test
        output <- list("test_stat" = test_stat,
                       "df" = df,
                       "p_value" = p_val,
                       "alternative" = alternative)
        return(output)
}
