test_that("Bayes posterior estimate for length of stay", {
  blos1 <- Bayes(losmcmc, y="post", parameter="muOfY", newdata=TRUE,
                 compare=4.5, rope=c(1,3))
  expect_equal(round(blos1$Posterior.Summary$Mean, 3),
               4.429)
})

test_that("Bayes posterior estimate of target values", {
  btarget1 <- Bayes(x=losmcmc, y="target", type="n", parameter=c("muOfY","sigmaOfY"),
                    newdata=TRUE, target=list(p=c(.35,.4,.45, .5, .55),  y=c(3,4)))
  expect_equal(round(btarget1$Target$Est.Quantile.P$Percentile_0.35[["Mode"]], 3),
               3.687)
})
