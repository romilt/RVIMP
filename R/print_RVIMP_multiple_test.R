#' Prints the main results from a RVIMP_multiple_test object
#' @description Summarizes the main results from a RVIMP_test object
#' @param x \code{RVIMP_multiple_test} object.
#' @param ... Further arguments to the \link{print} function.
#' @export

print.RVIMP_multiple_test<-function(x,...){
  cat("RVIMP multiple test result\n\n")
  cat("Used multiple test procedure:                ", x$method, "\n")
  cat("Variables checked for dependencies:          ", names(x$multiple_tests), "\n")
  cat("Niveau for type I error (\U03B1 niveau):          " , round(sapply(x$multiple_tests,function(i){i$alpha}),4), "\n")
  cat("Test results:                                ", sapply(x$multiple_tests,function(i){i$test}), "\n")
  cat("P Values:                                    ", sapply(x$multiple_tests,function(i){i$p}), "\n")
  cat("RVIMPs:                                      ", round(sapply(x$multiple_tests,function(i){i$RVIMP}),5), "\n")
  cat("VIMPs:                                       ", round(sapply(x$multiple_tests,function(i){i$VIMP}),5), "\n")
  cat("Decision quantils:                           ", round(sapply(x$multiple_tests,function(i){i$Quantil}),5), "\n")
}
