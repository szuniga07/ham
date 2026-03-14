car_m1 <- assess(formula=vs ~ hp + am, data=mtcars, regression="logistic")
d1 <- decide(x=car_m1, threshold= -0.767)
test_that("Logistic model AUC predicting engine shape and a classification threshold of logit= -0.18", {
  expect_equal(round(d1$AUC, 3), 0.881)
})

test_that("Net benefit when selecting the classification threshold as the 50th percentile of predicted values", {
  expect_equal(round(d1$DCA$Net.Benefit[[5]], 3), 0.363)
})
