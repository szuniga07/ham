test_that("This model should return 'review.ols' class", {
  expect_equal(class(review(assess(mpg ~ hp + wt + cyl, data=mtcars, regression= "ols")$model)),
               c("review","ham","list"))
})
