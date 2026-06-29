test_that("This model should return 'Summary.ols' class", {
  expect_equal(class(Summary(assess(mpg ~ hp + wt + cyl, data=mtcars, regression= "ols")$model)),
               c("Summary","ham","list"))
})
