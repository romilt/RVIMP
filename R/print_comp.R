#' Print results for comparison between VIMPs and RVIMPs
#' @description Summarizes the results of the comparison between VIMPs and RVIMPs
#' @param x \code{RVIMP_comp} object.
#' @param ... Further arguments to the \link{print} function.
#' @export

print.RVIMP_comp<-function(x,...){
  cat("Comparison VIMPs and RVIMPs \n\n")
  cat("Type of residual model:",x$residual.model, "\n")
  cat("Variables:             ",rownames(x$table), "\n")
  cat("RVIMPs:                ",round(x$table[,2],5), "\n")
  cat("VIMPs:                 ",round(x$table[,1],5), "\n")
}
