#' Plot the results of a RVIMP object
#' @description Generates the plot to compare VIMPs from model_A and model_B with for every variable checked for dependencies.
#' @param x \code{RVIMP} object.
#' @param which For which variable shall the plot be generated. Either a \link{vector} with the names of the desired variables or \code{"all"} to generate the plot for each variable the RVIMPs were calculated for.
#' @param ask Logical. If \code{TRUE} the user is asked to press Enter before a new figure is drawn. If \code{FALSE} all figures are drawn at once.
#' @param ... Further arguments to the \link{plot} function.
#' @export

plot.RVIMP<-function(x,which="all",ask=T,...){
  obj<-x
  org_vimp<-obj$VIMP
  if((sum(!(which %in% obj$confounded))>0))
  {
    if(length(which)==1)
    {
      if((which!="all"))
      {
        stop(paste("Error: Variable(s)",which[which(!(which %in% obj$confounded))], "are/is not part of the RVIMP object."))
      }
    } else {
      stop(paste("Error: Variable(s)",which[which(!(which %in% obj$confounded))], "are/is not part of the RVIMP object."))
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
          conf<-obj$confounded
          tmp<-data.frame(name=factor(rep(names(org_vimp),2),levels = names(org_vimp)),model=c(rep("mod_A",length(org_vimp)),rep("mod_B",length(org_vimp))),VIMP=c(org_vimp,obj$RVIMPs[[conf]]$model$variable.importance))
          ggplot2::ggplot(data = tmp,ggplot2::aes(x=name,y=VIMP,col=model))+ggplot2::geom_point()+
            ggplot2::scale_color_discrete(name = "Model", labels = c(expression(Model[A]),expression(Model[B])))+
            ggplot2::xlab("Variable")+ggplot2::ggtitle(label = paste("Comparison for Variable",conf))
        } else {
          graphics::par(ask=T)
          lapply(obj$RVIMPs,function(i){
            tmp<-data.frame(name=factor(rep(names(org_vimp),2),levels = names(org_vimp)),model=c(rep("mod_A",length(org_vimp)),rep("mod_B",length(org_vimp))),VIMP=c(org_vimp,i$model$variable.importance))
            ggplot2::ggplot(data = tmp,ggplot2::aes(x=name,y=VIMP,col=model))+ggplot2::geom_point()+
              ggplot2::scale_color_discrete(name = "Model", labels = c(expression(Model[A]),expression(Model[B])))+
              ggplot2::xlab("Variable")+ggplot2::ggtitle(label = paste("Comparison for Variable",i$conf))
          })
        }
      } else {
        tmp<-data.frame(name=factor(rep(names(org_vimp),2),levels = names(org_vimp)),model=c(rep("mod_A",length(org_vimp)),rep("mod_B",length(org_vimp))),VIMP=c(org_vimp,obj$RVIMPs[[which]]$model$variable.importance))
        ggplot2::ggplot(data = tmp,ggplot2::aes(x=name,y=VIMP,col=model))+ggplot2::geom_point()+
          ggplot2::scale_color_discrete(name = "Model", labels = c(expression(Model[A]),expression(Model[B])))+
          ggplot2::xlab("Variable")+ggplot2::ggtitle(label = paste("Comparison for Variable",obj$RVIMPs[[which]]$conf))
      }
    } else {
      graphics::par(ask=T)
      lapply(which,function(i){
        tmp<-data.frame(name=factor(rep(names(org_vimp),2),levels = names(org_vimp)),model=c(rep("mod_A",length(org_vimp)),rep("mod_B",length(org_vimp))),VIMP=c(org_vimp,obj$RVIMPs[[i]]$model$variable.importance))
        ggplot2::ggplot(data = tmp,ggplot2::aes(x=name,y=VIMP,col=model))+ggplot2::geom_point()+
          ggplot2::scale_color_discrete(name = "Model", labels = c(expression(Model[A]),expression(Model[B])))+
          ggplot2::xlab("Variable")+ggplot2::ggtitle(label = paste("Comparison for Variable",obj$RVIMPs[[i]]$conf))
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
          conf<-obj$confounded
          tmp<-data.frame(name=factor(rep(names(org_vimp),2),levels = names(org_vimp)),model=c(rep("mod_A",length(org_vimp)),rep("mod_B",length(org_vimp))),VIMP=c(org_vimp,obj$RVIMPs[[conf]]$model$variable.importance))
          ggplot2::ggplot(data = tmp,ggplot2::aes(x=name,y=VIMP,col=model))+ggplot2::geom_point()+
            ggplot2::scale_color_discrete(name = "Model", labels = c(expression(Model[A]),expression(Model[B])))+
            ggplot2::xlab("Variable")+ggplot2::ggtitle(label = paste("Comparison for Variable",conf))
        } else {
          lapply(obj$RVIMPs,function(i){
            tmp<-data.frame(name=factor(rep(names(org_vimp),2),levels = names(org_vimp)),model=c(rep("mod_A",length(org_vimp)),rep("mod_B",length(org_vimp))),VIMP=c(org_vimp,i$model$variable.importance))
            ggplot2::ggplot(data = tmp,ggplot2::aes(x=name,y=VIMP,col=model))+ggplot2::geom_point()+
              ggplot2::scale_color_discrete(name = "Model", labels = c(expression(Model[A]),expression(Model[B])))+
              ggplot2::xlab("Variable")+ggplot2::ggtitle(label = paste("Comparison for Variable",i$conf))
          })
        }
      } else {
        tmp<-data.frame(name=factor(rep(names(org_vimp),2),levels = names(org_vimp)),model=c(rep("mod_A",length(org_vimp)),rep("mod_B",length(org_vimp))),VIMP=c(org_vimp,obj$RVIMPs[[which]]$model$variable.importance))
        ggplot2::ggplot(data = tmp,ggplot2::aes(x=name,y=VIMP,col=model))+ggplot2::geom_point()+
          ggplot2::scale_color_discrete(name = "Model", labels = c(expression(Model[A]),expression(Model[B])))+
          ggplot2::xlab("Variable")+ggplot2::ggtitle(label = paste("Comparison for Variable",obj$RVIMPs[[which]]$conf))
      }
    } else {
      lapply(which,function(i){
        tmp<-data.frame(name=factor(rep(names(org_vimp),2),levels = names(org_vimp)),model=c(rep("mod_A",length(org_vimp)),rep("mod_B",length(org_vimp))),VIMP=c(org_vimp,obj$RVIMPs[[i]]$model$variable.importance))
        ggplot2::ggplot(data = tmp,ggplot2::aes(x=name,y=VIMP,col=model))+ggplot2::geom_point()+
          ggplot2::scale_color_discrete(name = "Model", labels = c(expression(Model[A]),expression(Model[B])))+
          ggplot2::xlab("Variable")+ggplot2::ggtitle(label = paste("Comparison for Variable",obj$RVIMPs[[i]]$conf))
      })
    }
  }
}
