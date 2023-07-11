#' Generates the test desicions criteria plot based on RVIMP distribution for multiple test procedure
#' @description Generates the test desicions criteria plot based on RVIMP distribution for variables checked for dependencies
#' @param x \code{RVIMP_multiple_test} object.
#' @param which For which variable shall the plot be generated. Either a \link{vector} with the names of the desired variables or \code{"all"} to generate the plot for each variable tested for dependencies.
#' @param ... Further arguments to the \link{plot} function.
#' @author Robert Miltenberger
#' @export

plot.RVIMP_multiple_test<-function(x,which="all",...){
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

  if(length(which)==1)
  {
    if(which=="all")
    {
      if(length(obj$multiple_tests)==1)
      {
        conf<-obj$variables
        obj$multiple_tests[[conf]]$plot
      } else {
        graphics::par(ask=T)
        lapply(obj$multiple_tests,function(i){
          i$plot
        })
      }
    } else {
      obj$multiple_tests[[which]]$plot
    }
  } else {
    graphics::par(ask=T)
    lapply(which,function(i){
      obj$multiple_tests[[i]]$plot
    })
  }
}
