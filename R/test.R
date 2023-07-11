#' Test procedure to check for dependencies within covariates based on Permutation Variable Importance Measures in Random Forests
#' @description Performes a resampling test to check if a variables importance is at least partially induced from other variables within a Random Forest.
#' @param formula Object of class formula or character describing the model to fit.
#' @param ranger.obj Trained random forest object of class \link{ranger}. If no object is passed \code{formula} is required to generate a new one. Passed \code{ranger} object must contain \code{variable.importance}.
#' @param data Training data of class data.frame.
#' @param Z Name of all variables that shall be checked for dependencies. If \code{Z="all"} the procedure is done for all independed variables from \code{formula}.
#' @param residual.model Model the residuals of \code{Z} shall be estimated with. Either \code{"Linear"} or \code{"RandomForest"} if residuals shall be estimated within the algorithm or \code{"Precalculated"} if precalculated residuals are passed to \code{residuals} by the user.
#' @param residuals Residuals, if they are precalculated.
#' @param alpha.one.sided Type I error probability.
#' @param keep.ranger Logical. Shall either the passed \code{ranger.obj} or the newly generated one be part of the result.
#' @param reps Number of respamplings to estimate the RVIMP distribution on. Each resampling is based on a data set with a sample size of 0.632 compared to the original data set.
#' @param sample.seed Seed to draw the \code{reps} resamplings of the original data set. Must be set for reproducible results.
#' @param seed Seed passed to ranger function for generating the Random Forest. Must be set for reproducible results. Note: \code{seed} is passed to the \code{ranger} function to generate the Random Forest, whereas \code{sample.seed} is needed to draw the resamplings for the test procedure.
#' @param num.trees Number of trees within each ranger object.
#' @param ... Further arguments to the \link{ranger} function.
#' @return Object of class \code{RVIMP_test} with elements
#'   \item{\code{call}}{Function call.}
#'   \item{\code{variables}}{Name of the variables tested for dependencies.}
#'   \item{\code{reps}}{Number of resamplings to approximate the RVIMP distribution.}
#'   \item{\code{alpha}}{Desired type I error rate.}
#'   \item{\code{residual.model}}{Model the residuals were bulit with.}
#'   \item{\code{ranger}}{Object of class \code{ranger} for the original model or \code{NULL} if \code{keep.ranger=F}.}
#'   \item{\code{RVIMPs}}{List of test results for each tested variable.}
#'   \item{\code{VIMP}}{Permutation Variable Importance of the original model.}
#' @author Robert Miltenberger
#' @examples
#' test.RVIMP(y~.,data=RVIMP_sim_data,Z="x1")
#' @seealso \code{\link{RVIMP}} \code{\link{multiple_test.RVIMP}}
#' @export

test.RVIMP<-function(formula=NULL,ranger.obj=NULL,data=NULL,Z="all",residual.model="Linear",residuals=NULL,alpha.one.sided=0.05,keep.ranger=T,reps=100,sample.seed=NULL,seed=NULL,num.trees=500,...)
{
  if(length(Z)<1)
  {
    stop("Error: At least one variable has to be chosen for procedure.")
  }
  if(length(Z)>1 & !is.null(residuals))
  {
    stop("Error: If precalculated residuals are passed please just specify one variable for Z.")
  }
  if(!is.null(ranger.obj)&!is.null(formula))
  {
    ranger.obj<-NULL
    warning("Warning: Passed ranger object is negleced because both a ranger object and a formula were passed.")
  }
  if(residual.model!="Precalculated")
  {
    if(sum(residual.model %in% c("Linear","RandomForest"))!=1)
    {
      stop("Error: residual.model must either be Linear or RandomForest.")
    }
    if(!is.null(residuals))
    {
      residual.model<-"Precalculated"
      warning("Warning: Both precalculated residuals and a model to estimate the residuals on were passed. Therefore the model is negleced and the passed residuals are used.")
    }
    if(Z=="all" & residual.model=="Precalculated")
    {
      stop("Error: If precalculated residuals are pass please just specify one variable for Z.")
    }
  } else {
    if(Z=="all")
    {
      stop("Error: If precalculated residuals are pass please just specify one variable for Z.")
    }
    if(is.null(residuals))
    {
      stop("Error: Please either specify the model to estimate the residuals with or pass precalculated residuals.")
    }
  }
  if(!is.null(ranger.obj))
  {
    if(sum(is.na(ranger.obj$variable.importance))!=0)
    {
      ranger.obj<-NULL
      warning("Warning: passed ranger object is negleced because it did not contain variable importance.")
    }
    if(ranger.obj$importance.mode!="permutation")
    {
      ranger.obj<-NULL
      warning("Warning: passed ranger object is negleced because other importance mode than permutation was used.")
    }
    if(ranger.obj$num.trees!=num.trees)
    {
      ranger.obj<-NULL
      warning("Warning: passed ranger object is negleced because number of trees differ between object and passed parameter to the funtion.")
    }
  }
  if (is.null(sample.seed)){
    sample.seed <- ceiling(stats::runif(1 , 0, 100))
  }
  if (is.null(seed)){
    seed <- stats::runif(1 , 0, .Machine$integer.max)
  }
  if(is.null(ranger.obj))
  {
    if(class(formula)!="formula") stop("Error: Either suitable ranger object or formula needed.")
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"),
               names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    cov<-attr(mt,"term.labels")
    mf<-mf[,c(deparse(formula[[2]]),cov)]
    original.names<-colnames(mf)
    if(length(Z)==1){
      if(Z=="all")
      {
        Z<-cov
      }
    }
    if(sum(deparse(formula[[2]]) %in% Z)==1)
    {
      Z<-Z[-which(Z==deparse(formula[[2]]))]
      warning("Warning: Response Variable was selected within Z and therefore removed.")
    }
    if(sum(!(Z %in% original.names))!=0)
    {
      stop(paste("Error: Variable(s)",Z[which(!(Z %in% colnames(mf)))], "are/is not part of the passed formula."))
    }
    colnames(mf)<-c("y",original.names[-1])
    ranger.obj<-ranger::ranger(formula=y~.,data=mf,importance = "permutation",seed=seed,num.trees=num.trees,...)
  } else {
    mf<-stats::model.frame(stats::as.formula(ranger.obj$call[[2]]),data=data,drop.unused.levels = T)
    original.names<-colnames(mf)
    if(length(Z)==1){
      if(Z=="all")
      {
        Z<-original.names[-1]
      }
    }
    if(sum(!(Z %in% original.names[-1]))!=0)
    {
      stop(paste("Error: Variable(s)",Z[which(!(Z %in% colnames(mf)))], "are/is not part of the passed formula."))
    }
    colnames(mf)<-c("y",original.names[-1])
  }

  compare_data<-get_distribution(data=mf,variables = Z,rep = reps,sample.seed = sample.seed,residual.model=residual.model,residual=residuals,num.trees=num.trees)
  rownames(compare_data)<-Z

  RVIMP_test_obj<-lapply(Z,RVIMP_test_model,data=mf,dens_data=compare_data,org.names=original.names,mod.type=residual.model,alpha=alpha.one.sided,rang.obj=ranger.obj,seed=seed,residual=residuals,num.trees=num.trees,...=...)
  names(RVIMP_test_obj)<-Z
  if(keep.ranger)
  {
    RVIMP_test_obj<-list(call=sys.call(),variables=Z,reps=reps,alpha=alpha.one.sided,residual.model=residual.model,ranger=ranger.obj,RVIMPs=RVIMP_test_obj,VIMP=ranger.obj$variable.importance[Z])
  } else {
    RVIMP_test_obj<-list(call=sys.call(),variables=Z,reps=reps,alpha=alpha.one.sided,residual.model=residual.model,ranger=NULL,RVIMPs=RVIMP_test_obj,VIMP=ranger.obj$variable.importance[Z])
  }
  class(RVIMP_test_obj)<-c("RVIMP_test")
  RVIMP_test_obj
}
