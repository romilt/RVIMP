#' Prints the main results from a RVIMP_test object
#' @description Summarizes the main results from a RVIMP_test object
#' @param x \code{RVIMP_test} object.
#' @param ... Further arguments to the \link{print} function.
#' @author Robert Miltenberger
#' @export

print.RVIMP_test<-function(x,...){
  cat("RVIMP test result\n\n")
  cat("Call:\n",deparse(x$call), "\n\n")
  cat("Number of resamplings for density estimation:", x$reps, "\n")
  cat("Niveau for type I error (\U03B1 niveau):          " , x$alpha, "\n")
  cat("Type of residual model:                      ", x$residual.model, "\n")
  cat("Variables:                                   ", x$variables, "\n")
  cat("Test results for Dependencies:               ", sapply(x$RVIMPs,function(i){i$test_dep}), "\n")
  cat("P Values for Dependencies:                   ", sapply(x$RVIMPs,function(i){i$p_dep}), "\n")
  cat("Decision quantiles:                          ", round(sapply(x$RVIMPs,function(i){i$Quantil}),5), "\n")
  cat("Test results for Influence:                  ", sapply(x$RVIMPs,function(i){i$test_inf}), "\n")
  cat("P Values for Influence:                      ", sapply(x$RVIMPs,function(i){i$p_inf}), "\n")
  cat("RVIMPs:                                      ", round(sapply(x$RVIMPs,function(i){i$RVIMP}),5), "\n")
  cat("VIMPs:                                       ", round(x$VIMP,5), "\n")
}
