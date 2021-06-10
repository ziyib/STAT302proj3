#' k-Nearest Neighbors Cross-Validation
#'
#' This function predict output using k-Nearest Neighbors Cross-Validation algorithms.
#'
#' @param train input data frame
#' @param cl true class value of your training data
#' @param k_nn integer representing the number of neighbors
#' @param k_cv integer representing the number of folds
#'
#' @import dplyr class tidyr
#'
#'
#' @keywords prediction
#'
#' @return return a list with objects class (a vector of the predicted class for all observation)
#' and cv_err as the numeric with the cross-validation misclassification error
#'
#' @examples
#' my_penguins <- tidyr::drop_na(my_penguins)
#' my_result_1 <- my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 1, k_cv = 5 )
#' my_result_2 <- my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 5, k_cv = 5 )
#' train_err_1 <- sum(as.numeric(my_penguins$species != my_result_1$class)) / nrow(my_penguins)
#' train_err_2 <- sum(as.numeric(my_penguins$species != my_result_2$class)) / nrow(my_penguins)
#' my_table_row_1 <- cbind("k_nn = 1" = my_result_1$cv_err, "k_nn = 5" = my_result_2$cv_err)
#' my_table_row_2 <- cbind("k_nn = 1" = train_err_1, "k_nn = 5" = train_err_2)
#' my_table <- rbind(my_table_row_1, my_table_row_2)
#' rownames(my_table) <- c("cv_err", "training_err")
#' my_table
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
        #Split data in k_cv parts, randomly
        split <-
                sample(rep(1:k_cv, length = nrow(train)), replace = TRUE)
        #set up a new column in penguins as "split"
        train$split <- split
        x <-train %>% select(bill_length_mm,
                                    bill_depth_mm,
                                    flipper_length_mm,
                                    body_mass_g)
        y <- train$species
        k_data_frame <-
                data.frame("x" = x,
                           "species" = y,
                           "split" = split)
        #Empty matrix to store predictions
        prediction_matrix <- matrix(NA, nrow(k_data_frame), 2)
        a <- vector(length = k_cv)
        for (i in 1:k_cv) {
                data_train <- k_data_frame %>% filter(split != i)
                data_test <- k_data_frame %>% filter(split == i)
                cl_train <- data_train %>% pull(species)
                cl_test <- data_test %>% pull(species)
                cl_species <- k_data_frame$species
                a[i] <- nrow(data_test)
                cl_train <- as.factor(cl_train)
                cl_test <- as.factor(cl_test)

                #Train our model and record predictions and errors
                prediction_result <-
                        knn(train = data_train[, 1:4],
                                test = data_test[, 1:4],
                                cl = cl_train,
                                k = k_nn)
                prediction_matrix[(sum(a[1:i - 1]) + 1):sum(a[1:i]), 1] <-
                        prediction_result
                prediction_matrix[(sum(a[1:i - 1]) + 1):sum(a[1:i]), 2] <-
                        cl_test
        }
        sum <- 0
        for (i in 1:nrow(prediction_matrix)) {
                if (prediction_matrix[i, 1] != prediction_matrix[i, 2]) {
                        sum <- sum + 1
                }
        }
        cv_err <- sum / nrow(k_data_frame)
        class <-knn(
                        train = k_data_frame[, 1:4],
                        test = k_data_frame[, 1:4],
                        cl = cl_species,
                        k = k_nn)
        return(list("class" = class, "cv_err" = cv_err))

}
