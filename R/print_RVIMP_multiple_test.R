#' Prints the main results from a RVIMP_multiple_test object
#' @description Summarizes the main results from a RVIMP_test object
#' @param x \code{RVIMP_multiple_test} object.
#' @param ... Further arguments to the \link{print} function.
#' @author Robert Miltenberger
#' @export

print.RVIMP_multiple_test<-function(x,...){
  cat("RVIMP multiple test result\n\n")
  cat("Used multiple test procedure:                ", x$method, "\n")
  cat("Variables:                                   ", names(x$multiple_tests), "\n")
  cat("Local niveau for type I error (\U03B1 niveau):    " , round(sapply(x$multiple_tests,function(i){i$alpha}),4), "\n")
  cat("Test results for Dependencies:               ", sapply(x$multiple_tests,function(i){i$test}), "\n")
  cat("P Values for Dependencies:                   ", sapply(x$multiple_tests,function(i){i$p}), "\n")
  cat("Decision quantiles:                          ", round(sapply(x$multiple_tests,function(i){i$Quantil}),5), "\n")
  cat("Test results for Influence:                  ", sapply(x$multiple_tests,function(i){i$test_inf}), "\n")
  cat("P Values for Influence:                      ", sapply(x$multiple_tests,function(i){i$p_inf}), "\n")
  cat("RVIMPs:                                      ", round(sapply(x$multiple_tests,function(i){i$RVIMP}),5), "\n")
  cat("VIMPs:                                       ", round(sapply(x$multiple_tests,function(i){i$VIMP}),5), "\n")
}
