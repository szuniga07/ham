test_that("X-bar chart upper control limit mean when assuming equal samples sizes", {
  expect_equal(round(mean(control(x="los", time="month", data=hosprog, type="x", n.equal=TRUE)$UCL), 3),
               5.193)
})

test_that("p-chart lower control limit mean when assuming equal samples sizes", {
  expect_equal(round(mean(control(x="rdm30", time="month", data=hosprog, type="p", n.equal=TRUE)$LCL), 3),
               0.03)
})
