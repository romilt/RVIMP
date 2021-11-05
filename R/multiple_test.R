#' Adjustment for multiple test procedure
#' @description Adjustment for multiple test procedure
#' @param test.obj Object of class test_RVIMP.
#' @param method Which procedure shall be applied for adjustment? Either "\code{Bonferoni}", "\code{Sidak}" or "\code{Holm}".
#' @return Object of class \code{RVIMP_multiple_test} with elements
#'  \item{\code{method}}{With multiple test procedure was used.}
#'  \item{\code{confounded}}{Name of the variables tested for dependencies.}
#'  \item{\code{multiple_tests}}{List with results for multiple test procedure for desired variables \code{confounded}.}
#' @examples
#' RVIMP_test_object<-test.RVIMP(y~.,data=RVIMP_sim_data,Z=c("x1","x2"))
#' multiple_test.RVIMP(RVIMP_test_object,method="Bonferroni")
#' @export

multiple_test.RVIMP<-function(test.obj=NULL,method="Bonferroni")
{
  if(class(test.obj)!="RVIMP_test")
  {
    stop("Error: Passed object has to be of class RVIMP_test")
  }
  if(sum(method %in% c("Bonferroni","Sidak","Holm"))==0)
  {
    stop("Error: Please use one of the three possible methods Bonferroni, Sidak or Holm")
  }
  num_tests<-length(test.obj$RVIMPs)
  if(num_tests==1)
  {
    stop("Error: Only one test was performed so no multiple test procedure is required")
  }
  conf<-names(test.obj$RVIMPs)
  alpha<-test.obj$alpha
  if(method=="Bonferroni")
  {
    res<-lapply(test.obj$RVIMPs,Bonferroni,alpha.new=alpha/num_tests)
  } else if(method=="Sidak"){
    res<-lapply(test.obj$RVIMPs,Sidak,alpha.new=1-(1-alpha)^(1/num_tests))
  } else {
    pvalues<-sapply(test.obj$RVIMPs,function(i){i$p})
    pvalues_sort<-sort(pvalues)
    res<-mapply(Holm,name=names(pvalues_sort),alpha=alpha/(num_tests:1),MoreArgs=list(obj=test.obj$RVIMPs),SIMPLIFY = F)
  }
  res<-list(method=method,confounded=conf,multiple_tests=res)
  class(res)<-"RVIMP_multiple_test"
  res
}
