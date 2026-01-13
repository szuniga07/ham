#Checking text produced
hos3 <- assess(formula=los ~ ., data=hosprog, intervention = "program",
               int.time="month", its="two", interrupt = 5)
ieff1 <- itsEffect(hos3$ITS, "mgst", interruptions = 1)
test_that("ITS effect: coefficient from two groups and 1 interrupt", {
  expect_equal(round(ieff1$Effect, 4), c(-0.0874, 0.1932, -0.2806))
})
