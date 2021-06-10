
test_that("non_numeric k throws error", {
  expect_error(my_rf_cv("5"))
})

test_that("my_rf_cv returns a numeric Estimate", {
  expect_is(my_rf_cv(k = 5), "numeric")
})
