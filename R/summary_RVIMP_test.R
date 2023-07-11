#' Summary of the results for a RVIMP_test for dependencies
#' @description Summary of the results for a RVIMP_test for dependencies
#' @param object \code{RVIMP_test} object.
#' @param ... Further arguments to the \link{summary} function.
#' @author Robert Miltenberger
#' @export

summary.RVIMP_test<-function(object,...){
  cat("Summary RVIMP test \n\n")
  res<-data.frame(test=sapply(object$RVIMPs,function(i){i$test}),p=sapply(object$RVIMPs,function(i){i$p}),RVIMP=round(sapply(object$RVIMPs,function(i){i$RVIMP}),5),VIMP=round(sapply(object$RVIMPs,function(i){i$VIMP}),5))
  rownames(res)<-object$confounded
  res
}
