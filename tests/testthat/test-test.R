library(RVIMP)
context("RVIMP.RVIMP_test")

rvimp_test<-test.RVIMP(y~.,Z="x1",data=RVIMP_sim_data,keep.ranger = T,num.trees=50)

test_that("test.RVIMP works for example data set", {
  expect_is(rvimp_test, "RVIMP_test")
})

test_that("test.RVIMP works for example data set", {
  expect_is(rvimp_test$ranger, "ranger")
})

test_that("test.RVIMP works for example data set", {
  expect_is(rvimp_test$call, "call")
})

test_that("test.RVIMP works for example data set", {
  expect_is(rvimp_test$variables, "character")
})

test_that("test.RVIMP works for example data set", {
  expect_is(rvimp_test$reps, "numeric")
})

test_that("test.RVIMP works for example data set", {
  expect_is(rvimp_test$alpha, "numeric")
})

test_that("test.RVIMP works for example data set", {
  expect_is(rvimp_test$residual.model, "character")
})

test_that("test.RVIMP works for example data set", {
  expect_is(rvimp_test$RVIMPs, "list")
})

test_that("test.RVIMP works for example data set", {
  expect_is(rvimp_test$VIMP, "numeric")
})
