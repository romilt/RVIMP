#' Comparison between classical VIMPs and RVIMPs
#' @description AUCH HIER SOLLTEN WIR UNS NOCH ETWAS AUSDENKEN
#' @param formula Object of class formula or character describing the model to fit.
#' @param ranger.obj Trained random forest object of class \code{ranger}. If no object is passed \code{formula} is used to generate a new one.
#' @param data Training data of class data.frame.
#' @param residual.model Type the residuals for each variable shall be based on. Either "Linear" or "RandomForest".
#' @param ... Additional arguments to the \link{ranger} function.
#' @return Object of class \code{RVIMP_comp} with elements
#'   \item{\code{residual.model}}{Model the residuals were built with.}
#'   \item{\code{table}}{Table containing the corresponding values for VIMPs and RVIMPs.}
#'   \item{\code{plot}}{Plot for visualised presentation of the comparison.}
#' @author Robert Miltenberger
#' @examples
#' compare_VIMP_RVIMP(y~.,data=RVIMP_sim_data,residual.model="Linear")
#' @export

compare_VIMP_RVIMP<-function(formula=NULL,ranger.obj=NULL,data=NULL,residual.model="Linear",...)
{
  if(!is.null(ranger.obj)&!is.null(formula))
  {
    warning("Warning: Passed ranger object is negleced because both a ranger object and a formula were passed.")
  }
  if(sum(residual.model %in% c("Linear","RandomForest"))!=1)
  {
    stop("Error: residual.model must either be Linear or RandomForest")
  }
  #create ranger object if needed

  if(is.null(ranger.obj))
  {
    if(class(formula)!="formula") stop("Error: Either ranger object or formula needed.")
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"),
               names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    original.names<-colnames(mf)

    colnames(mf)<-c("y",original.names[-1])
    ranger.obj<-ranger::ranger(formula=y~.,data=mf,importance = "permutation",...)
  } else {
    mf<-stats::model.frame(stats::as.formula(ranger.obj$call[[2]]),data=data,drop.unused.levels = T)
    original.names<-colnames(mf)
    colnames(mf)<-c("y",original.names[-1])
  }

  RVIMP_obj<-lapply(original.names[-1],RVIMP_model,data=mf,orig.names=original.names,mod.type=residual.model,...=...)
  comp_tab<-cbind(ranger.obj$variable.importance,sapply(RVIMP_obj,function(x){x$RVIMP}))
  colnames(comp_tab)<-c("VIMPs","RVIMPs")
  plot_dat<-data.frame(factor(rep(rownames(comp_tab),2),levels =rownames(comp_tab)),factor(c(rep("VIMP",length(rownames(comp_tab))),rep("RVIMP",length(rownames(comp_tab)))),levels = c("VIMP","RVIMP")),as.numeric(comp_tab))
  colnames(plot_dat)<-c("name","type","VIMP")
  p<-ggplot2::ggplot(data = plot_dat,ggplot2::aes(x=name,y=VIMP,col=type))+ggplot2::geom_point()+
    ggplot2::scale_color_discrete(name = "Model", labels = c(expression(Model[A]),expression(Model[B])))+
    ggplot2::xlab("Variable")+ggplot2::ggtitle(label = "Comparison between VIMP and RVIMP for all variables")
  comp_dat<-list(residual.model=residual.model,table=comp_tab,plot=p)
  class(comp_dat)<-"RVIMP_comp"
  comp_dat
}
