#' Generates the test desicions criteria plot based on RVIMP distribution
#' @description Generates the test desicions criteria plot based on RVIMP distribution for variables checked for dependencies
#' @param x \code{RVIMP_test} object.
#' @param which For which variable shall the plot be generated. Either a \link{vector} with the names of the desired variables or \code{"all"} to generate the plot for each variable tested for dependencies.
#' @param ask Logical. If \code{TRUE} the user is asked to press Enter before a new figure is drawn. If \code{FALSE} all figures are drawn at once.
#' @param ... Further arguments to the \link{plot} function.
#' @export

plot.RVIMP_test<-function(x,which="all",ask=T,...){
  obj<-x
  if((sum(!(which %in% obj$variables))>0))
  {
    if(length(which)==1)
    {
      if((which!="all"))
      {
        stop(paste("Error: Variable(s)",which[which(!(which %in% obj$variables))], "are/is not part of the RVIMP_test object."))
      }
    } else {
      stop(paste("Error: Variable(s)",which[which(!(which %in% obj$variables))], "are/is not part of the RVIMP_test object."))
    }
  }

  if(ask==T)
  {
    if(length(which)==1)
    {
      if(which=="all")
      {
        if(length(obj$RVIMPs)==1)
        {
          conf<-obj$variables
          obj$RVIMPs[[conf]]$plot
        } else {
          graphics::par(ask=T)
          lapply(obj$RVIMPs,function(i){
            i$plot
          })
        }
      } else {
        obj$RVIMPs[[which]]$plot
      }
    } else {
      graphics::par(ask=T)
      lapply(which,function(i){
        obj$RVIMPs[[i]]$plot
      })
    }
  } else {
    graphics::par(ask=F)
    if(length(which)==1)
    {
      if(which=="all")
      {
        if(length(obj$RVIMPs)==1)
        {
          conf<-obj$variables
          obj$RVIMPs[[conf]]$plot
        } else {
          lapply(obj$RVIMPs,function(i){
            i$plot
          })
        }
      } else {
        obj$RVIMPs[[which]]$plot
      }
    } else {
      lapply(which,function(i){
        obj$RVIMPs[[i]]$plot
      })
    }
  }
}
