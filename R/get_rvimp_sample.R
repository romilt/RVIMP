#' Get sample from RVIMP distribution from RVIMP_test object
#' @description Get sample from RVIMP distribution from RVIMP_test object
#' @param obj RVIMP_test object
#' @param which Variables the distribution shall be returned for
#' @return Matrix with samples from the RVIMP distribution the test procedure was performed with for desired variables
#' @export


get_rvimp_sample<-function(obj,which="all")
{
  if(length(which)==1)
  {
    if(which=="all")
    {
      which<-names(obj$RVIMPs)
    }
  }
  if(sum(!(which %in% names(obj$RVIMPs)))!=0)
  {
    stop("Error: At least one selected variable is not part of the RVIMP_test object")
  }
  dens<-sapply(obj$RVIMPs,function(x){x$dens_data})
  dens[,which]
}
