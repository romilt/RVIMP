library(RVIMP)
context("RVIMP_RVIMP")

rvimp<-RVIMP(y~.,Z="x1",data=RVIMP_sim_data,keep.ranger = T,num.trees=50)

test_that("RVIMP works for example data set", {
  expect_is(rvimp, "RVIMP")
})

test_that("RVIMP works for example data set", {
  expect_is(rvimp$ranger, "ranger")
})

test_that("RVIMP works for example data set", {
  expect_is(rvimp$call, "call")
})

test_that("RVIMP works for example data set", {
  expect_is(rvimp$variables, "character")
})

test_that("RVIMP works for example data set", {
  expect_is(rvimp$residual.model, "character")
})

test_that("RVIMP works for example data set", {
  expect_is(rvimp$RVIMPs, "list")
})

test_that("RVIMP works for example data set", {
  expect_is(rvimp$VIMP, "numeric")
})
