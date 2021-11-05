#' Prints the main results from a RVIMP_test object
#' @description Summarizes the main results from a RVIMP_test object
#' @param x \code{RVIMP_test} object.
#' @param ... Further arguments to the \link{print} function.
#' @export

print.RVIMP_test<-function(x,...){
  cat("RVIMP test result\n\n")
  cat("Call:\n",deparse(x$call), "\n\n")
  cat("Number of resamplings for density estimation:", x$reps, "\n")
  cat("Niveau for type I error (\U03B1 niveau):          " , x$alpha, "\n")
  cat("Type of residual model:                      ", x$residual.model, "\n")
  cat("Variables checked for dependencies:          ", x$variables, "\n")
  cat("Test results:                                ", sapply(x$RVIMPs,function(i){i$test}), "\n")
  cat("P Values:                                    ", sapply(x$RVIMPs,function(i){i$p}), "\n")
  cat("RVIMPs:                                      ", round(sapply(x$RVIMPs,function(i){i$RVIMP}),5), "\n")
  cat("VIMPs:                                       ", round(x$VIMP,5), "\n")
  cat("Decision quantils:                           ", round(sapply(x$RVIMPs,function(i){i$Quantil}),5), "\n")
}
