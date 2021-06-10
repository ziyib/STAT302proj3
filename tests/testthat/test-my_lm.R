# within test-my_lm.R
data(my_gapminder)
test_that("my_lm returns a numeric Estimate", {
        expect_is(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)$Estimate,"matrix")
})
 # test_that("my_lm returns a numeric Estimate", {
 #         expect_is(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)$Std..Error,"matrix")
 # })
 # test_that("my_lm returns a numeric t.value", {
 #         expect_is(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)$t.value,"matrix")
 # })
 #
 # test_that("my_lm returns a numeric Pr...t..", {
 #         expect_is(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)$Pr...t..,"data frame")
 # })

 test_that("my_lm returns a numeric data frame", {
         expect_is(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder),"list")
 })
