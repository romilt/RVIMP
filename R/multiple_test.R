#' Adjustment for a multiple comparison problem
#' @description Adjustment for a multiple comparison problem by applying either Bonferroni, Sidak, or Holm procedure to control the Family-wise error rate (FWER) or the False discovery rate (FDR).
#' @param test.obj \code{test_RVIMP} object.
#' @param method Which procedure shall be applied for adjustment? Either "\code{Bonferroni}", "\code{Sidak}" or "\code{Holm}".
#' @return Object of class \code{RVIMP_multiple_test} with elements
#'  \item{\code{method}}{Which multiple test procedure was performed.}
#'  \item{\code{variables}}{Name of the variables tested for dependencies.}
#'  \item{\code{multiple_tests}}{List with results for multiple test procedure for desired variables \code{variables}.}
#' @author Robert Miltenberger
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
    pvalues<-sapply(test.obj$RVIMPs,function(i){i$p_dep})
    pvalues_sort<-sort(pvalues)
    res<-mapply(Holm,name=names(pvalues_sort),alpha=alpha/(num_tests:1),MoreArgs=list(obj=test.obj$RVIMPs),SIMPLIFY = F)
  }
  res<-list(method=method,variables=conf,multiple_tests=res)
  class(res)<-"RVIMP_multiple_test"
  res
}
