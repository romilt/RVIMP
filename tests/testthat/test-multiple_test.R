library(RVIMP)
context("RVIMP.RVIMP_multiple_test")

rvimp_test<-test.RVIMP(y~.,Z=c("x1","x2"),data=RVIMP_sim_data,num.trees=50)
rvimp_multiple_test<-multiple_test.RVIMP(rvimp_test,method="Bonferroni")

test_that("multiple_test.RVIMP works for example data set", {
  expect_is(rvimp_multiple_test, "RVIMP_multiple_test")
})
