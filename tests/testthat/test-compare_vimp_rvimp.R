library(RVIMP)
context("RVIMP.compare_VIMP_RVIMP")

rvimp_comp<-compare_VIMP_RVIMP(y~.,data=RVIMP_sim_data)

test_that("compare_VIMP_RVIMP works for example data set", {
  expect_is(rvimp_comp, "RVIMP_comp")
})

test_that("compare_VIMP_RVIMP works for example data set", {
  expect_is(rvimp_comp$residual.model, "character")
})

test_that("compare_VIMP_RVIMP works for example data set", {
  expect_is(rvimp_comp$table, "matrix")
})

test_that("compare_VIMP_RVIMP works for example data set", {
  expect_is(rvimp_comp$plot, c("gg","ggplot"))
})
