library(RVIMP)
context("RVIMP_print_RVIMP")

rvimp<-RVIMP(y~.,Z="x1",data=RVIMP_sim_data,keep.ranger = T,num.trees=50)

test_that("print works for example data set", {
  expect_that(print(rvimp), prints_text("RVIMP result"))

})


