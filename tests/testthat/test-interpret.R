#Checking text produced
hos3 <- assess(formula=los ~ ., data=hosprog, intervention = "program",
               int.time="month", its="two", interrupt = 5)
int1 <- interpret(hos3)
test_that("ITS interpretation: coefficient from two groups and 1 interrupt", {
  expect_equal(grep("txp5 is 0.03", int1$its$B5), grep("txp5 is 0.03", "txp5 is 0.03"))
})
