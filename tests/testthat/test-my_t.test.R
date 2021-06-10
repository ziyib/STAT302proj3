#CHANGE THE LIBARY TO MY_GAPMINDER!
# within test-my_t.test
test_that("my_t.test produce numeric test_stat", {
        expect_is(my_t_test(x = my_gapminder$lifeExp,alternative = "two sided", mu = 60)$test_stat, "numeric")

})
test_that("my_t.test produce numeric degree of freedom", {
        expect_is(my_t_test(x = my_gapminder$lifeExp,alternative = "less", mu = 60)$df, "numeric")

})
test_that("my_t.test produce numeric degree of freedom", {
        expect_is(my_t_test(x = my_gapminder$lifeExp,alternative = "greater", mu = 60)$alternative, "character")

})
test_that("my_t.test input throws error", {
        expect_error(my_t_test(x = my_gapminder$lifeExp,alternative = "less", mu = "string")$df)

})
test_that("my_t.test input throws message", {
        expect_error(my_t_test(x = my_gapminder$lifeExp,alternative = "other", mu = 60)$df, "please try again")

})
test_that("my_t.test gives a p-value", {
        expect_is(my_t_test(x = my_gapminder$lifeExp,alternative = "greater", mu = 60)$df, "numeric")

})
# test_that("my_t.test produce numeric degree of freedom", {
#         expect_is(my_t_test(x = my_gapminder$lifeExp,alternative = "less", mu = 60)$df, "numeric")
#
# })
# test_that("my_t.test produce numeric degree of freedom", {
#         expect_output(my_t_test(x = my_gapminder,alternative = "less", mu = 60), "4 variables")
# })
