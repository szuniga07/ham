#standard regression model tests (i.e., not DID or ITS)
test_that("ordinary least squares R^2", {
  expect_equal(round(summary(assess(mpg ~ hp+wt, data=mtcars, regression= "ols", topcode=30)$model)$r.squared, 5), 0.85951)
})

test_that("logistic: hp coefficient", {
  expect_equal(round(summary(assess(formula=vs~mpg+wt+hp, data=mtcars, regression="logistic")$model)$coef[4,1], 5), -0.09318)
})

#with propensity score
m1 <- assess(formula=los ~ month+program, data=hosprog, intervention = "program",
             regression="ols", propensity=c("female","age","risk"))$model
test_that("ordinary least squares: with propensity score coefficient", {
  expect_equal(round(coef(m1)[["pscore"]], 5), -11.74633)
})
#with propensity score and top coding and new data returned
m2 <- assess(formula=los ~ month+program, data=hosprog, intervention = "program",
             regression="ols", topcode=17.1, propensity=c("female","age","risk"))$model
test_that("ordinary least squares: intercept with top coding los and propensity score", {
  expect_equal(round(coef(m2)[["(Intercept)"]], 5), 10.26821)
})
#with propensity score and top coding and new data returned
m3 <- assess(formula=los ~ month+program, data=hosprog, intervention = "program",
             regression="ols", topcode=17.1, propensity=c("female","age","risk"),
             newdata=TRUE)
test_that("OLS: top coding los and propensity score means: top.los and pscore", {
  expect_equal(round(c(mean(m3$newdata[["pscore"]]), mean(m3$newdata[["top.los"]])), 5), c(0.48889, 4.42826) )
})

#differences-in-differences models
hos1 <- assess(formula=los ~ ., data=hosprog, intervention = "program",
               int.time="month", treatment = 5, did="two")
test_that("DID model: DID coefficient from 2 time points", {
  expect_equal(round(coef(hos1$DID)[["DID"]], 4), -3.5702)
})
hos2 <- assess(formula=los ~ ., data=hosprog, intervention = "program",
               int.time="month", treatment = 5, did="many")
test_that("DID model: DID.Trend coefficient from MANY time points", {
  expect_equal(round(coef(hos2$DID)[["DID.Trend"]], 4), -0.1932)
})

#interrupted time-series model
hos3 <- assess(formula=los ~ ., data=hosprog, intervention = "program",
               int.time="month", its="two", interrupt = 5)
test_that("ITS model: txip5 coefficient from two groups and 1 interrupt", {
  expect_equal(round(coef(hos3$ITS)[["txip5"]], 4), 0.1701)
})
hos4 <- assess(formula=los ~ ., data=hosprog, intervention = "program",
               int.time="month", its="two", interrupt = c(5,9))
test_that("ITS model: txip9 coefficient from two groups and 2 interruptions", {
  expect_equal(round(coef(hos4$ITS)[["txip9"]], 4), -0.0552)
})


