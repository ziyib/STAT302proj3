#' Random Forest Cross-Validation
#'
#'  This function predicts output class \code{body_mass_g} by covariates \code{bill_length_mm},
#'  \code{bill_depth_mm}, \code{flipper_length_mm} within \code{\link[palmerpenguins]{penguins}}
#'  dataset by conducting random forest cross-validation.
#'
#' @param k number of folds
#' @keywords prediction
#'
#' @return Numeric representing the cross-validation error.
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  dat <- penguins %>%
    select(body_mass_g, bill_length_mm,
           bill_depth_mm, flipper_length_mm)
  dat <- na.omit(dat)

  # split data
  dat$split <- sample(rep(1:k, length = nrow(dat)))
  # initialize prediction matrix
  predict_mat <- rep(NA, nrow(dat))

  mse <- matrix(NA, k, 1)

  # iterate through k-folds
  for (i in 1:k){
    # split the data
    data_train <- dat %>% filter(split != i)
    data_test <- dat %>% filter(split == i)
    # prediction model using random forest
    result <- randomForest(body_mass_g ~ bill_length_mm +
                             bill_depth_mm + flipper_length_mm,
                           data = data_train,
                           ntree = 100)
    mse[i, 1] <- mean((predict(result, data_test[, -1]) - data_test$body_mass_g)^2)
  }
  # return CV error
  return(list("MSE" = mean(mse)))
}
