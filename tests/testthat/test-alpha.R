#Alpha scale values from 5, 4, and 3 items
test_that("Alpha scale score for all 5 items", {
  expect_equal(round(alpha(items=c("i1","i2","i3","i4","i5"), data=cas)$Scale.Statistics$alpha, 2), .92)
})

test_that("Alpha scale score for all 4 items", {
  expect_equal(round(alpha(items=c("i2","i3","i4","i5"), data=cas)$Scale.Statistics$alpha, 2), .93)
})

test_that("Alpha scale score for all 3 items", {
  expect_equal(round(alpha(items=c("i3","i4","i5"), data=cas)$Scale.Statistics$alpha, 2), .91)
})
