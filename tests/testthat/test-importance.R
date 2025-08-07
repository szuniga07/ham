im1 <- importance(assess(mpg ~ hp+wt, data=mtcars, regression= "ols")$model)
test_that("importance chi-square of ordinary least squares model", {
  expect_equal(round(im1$Chi.Sq, 3) , c(12.381, 37.561))
})

im2 <- importance(assess(vs~mpg+wt+hp, data=mtcars, regression= "logistic")$model)
test_that("importance chi-square of logistic model", {
  expect_equal(round(im2$Chi.Sq, 3) , c(1.068, 1.475, 4.656))
})




