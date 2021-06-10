
my_penguins <- drop_na(my_penguins)

test_that("my_knn_cv returns a numeric Estimate", {
        expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 1, k_cv = 5 ),"list")
        expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 2, k_cv = 5 ),"list")
        expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 3, k_cv = 5 ),"list")
        expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 4, k_cv = 5 ),"list")
        expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 5, k_cv = 5 ),"list")
        expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 6, k_cv = 5 ),"list")
        expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 7, k_cv = 5 ),"list")
        expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 8, k_cv = 5 ),"list")
        expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 9, k_cv = 5 ),"list")
        expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 10, k_cv = 5 ),"list")
})

# test_that("my_knn_cv returns a numeric Estimate", {
#         expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 5, k_cv = 1 ),"list")
#         expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 5, k_cv = 2 ),"list")
#         expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 5, k_cv = 3 ),"list")
#         expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 5, k_cv = 4 ),"list")
#         expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 5, k_cv = 5 ),"list")
#         expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 5, k_cv = 6 ),"list")
#         expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 5, k_cv = 7 ),"list")
#         expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 5, k_cv = 8 ),"list")
#         expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 5, k_cv = 9 ),"list")
#         expect_is(my_knn_cv(train = my_penguins, cl = my_penguins$species, k_nn = 5, k_cv = 10 ),"list")
# })
