library(tidyr)
test_that("my_knn_cv returns a numeric Estimate", {
        my_penguins <- drop_na(my_penguins)
        expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 1:10, k_cv = 5 ),"list")
})

