test_that("Overall point estimation using t-distribution", {
  expect_equal(round(group(x="program", y="los", data=hosprog)$Group.CI$adf_all$PointEst, 3),
               4.428)
})

test_that("90% Point estimation using converted quartiles", {
  expect_equal(round(group(x="risk", y="rdm30", data=hosprog, quarts=TRUE, dist="b", conf.int=0.90)$Group.CI$adf_alpha$PointEst[[1]], 3),
               0.089)
})
