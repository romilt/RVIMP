#' Prints the main results from a RVIMP object
#' @description Summarizes the main results from a RVIMP object
#' @param x \code{RVIMP} object.
#' @param ... Further arguments to the \link{print} function.
#' @export

print.RVIMP<-function(x,...){
  cat("RVIMP result\n\n")
  cat("Call:\n",deparse(x$call), "\n\n")
  cat("Variables checked for dependencies:", x$confounded, "\n")
  cat("Type of residual model:            ", x$residual.model, "\n")
  cat("RVIMPs:                            ", round(sapply(x$RVIMPs,function(i){i$RVIMP}),5), "\n")
  cat("VIMPs:                             ", round(x$VIMP[x$variables],5), "\n")
}
