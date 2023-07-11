#' Summary of the results for a RVIMP_multiple_test for dependencies
#' @description Summary of the results for a RVIMP_multiple_test for dependencies
#' @param object \code{RVIMP_multiple_test} object.
#' @param ... Further arguments to the \link{summary} function.
#' @author Robert Miltenberger
#' @export

summary.RVIMP_multiple_test<-function(object,...){
  cat("Summary RVIMP multiple test with",object$method,"procedure\n\n")
  res<-data.frame(alpha=sapply(object$multiple_tests,function(i){i$alpha}),test=sapply(object$multiple_tests,function(i){i$test}),p=sapply(object$multiple_tests,function(i){i$p}),RVIMP=round(sapply(object$multiple_tests,function(i){i$RVIMP}),5),VIMP=round(sapply(object$multiple_tests,function(i){i$VIMP}),5))
  rownames(res)<-names(object$multiple_tests)
  res
}
