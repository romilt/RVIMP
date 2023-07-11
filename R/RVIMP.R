# -------------------------------------------------------------------------------
#   This file is part of RVIMP.
#
# RVIMP is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# RVIMP is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with RIVMP. If not, see <http://www.gnu.org/licenses/>.
#
# Written by:
#
# Robert Miltenberger
# Fachbereiche Informatik/Mathematik und Naturwissenschaften
# University of applied Sciences Darmstadt
# Schöfferstraße 3
# 64295 Darmstadt
# Germany
#
# -------------------------------------------------------------------------------
#' Estimation of permutation variable importance corrected for dependencies inbetween the covariates in Random Forests
#' @description RVIMP is a way to improve the interpretability of variables in Random Forests. The procedure is based on the permutation variable importance, first mentioned by Breiman 2001. This procedure aims to correct the permutation variable importance to partially are totally shared information within the set of covariates and therefore adjusts the resulting VIMPs to their actual influence on the prediction performance of the corresponding variable.
#' @param formula Object of class formula or character describing the model to fit.
#' @param ranger.obj Trained random forest object of class \link{ranger}. If no object is passed \code{formula} is used to generate a new one. Passed \code{ranger} object must contain \code{variable.importance}.
#' @param data Training data of class data.frame.
#' @param Z Name of all variables the RVIMPs shall be calculated for. If \code{Z="all"} the procedure is performed for all independed variables from \code{formula}.
#' @param residual.model Modeltype the residuals of \code{Z} shall be based on. Either \code{"Linear"} or \code{"RandomForest"}.
#' @param keep.ranger Logical. Shall generated ranger object be kept.
#' @param ... Further arguments to the \link{ranger} function. For reproducible results \code{seed} must be set.
#' @return Object of class \code{RVIMP} with elements
#'   \item{\code{call}}{Function call.}
#'   \item{\code{variables}}{Name of the variables the RVIMPs are calculated for.}
#'   \item{\code{residual.model}}{Model the residuals were bulit with.}
#'   \item{\code{ranger}}{Object of class \code{ranger} for the original model or \code{NULL} if \code{keep.ranger=F}.}
#'   \item{\code{RVIMPs}}{List of results for each variable the RVIMP shall be calculated for.}
#'   \item{\code{VIMP}}{Permutation Variable Importance of the original model.}
#' @author Robert Miltenberger
#' @references
#' \itemize{
#'   \item Breimann L. Random Forests, Mach. Learn. 2001; 45(1):5-32. \doi{https://doi.org/10.1023/A:1010933404324}.
#'   \item Gregorutti B, Saint-Pierre P. Correlation and variable importance in random forests, Stat Comput 2017; 27:659-678. \doi{https://doi.org/10.1007/s11222-016-9646-1}
#'   \item Wright MN, Ziegler A. ranger: A fast implementation of random forests for high dimensional data in C++ and R. J Stat Softw 2017, 77:1-17. \doi{https://doi.org/10.18637/jss.v077.i01}
#'  }
#' @examples
#' RVIMP(formula=y~.,data=RVIMP_sim_data,Z = "all",residual.model = "Linear")
#' @seealso \code{\link{ranger}} \code{\link{test.RVIMP}} \code{\link{compare_VIMP_RVIMP}}
#' @export

RVIMP<-function(formula=NULL,ranger.obj=NULL,data=NULL,Z="all",residual.model="Linear",keep.ranger=T,...)
{
  if(length(Z)<1)
  {
    stop("Error: At least one variable has to be chosen for procedure")
  }
  if(!is.null(ranger.obj)&!is.null(formula))
  {
    ranger.obj<-NULL
    warning("Warning: Passed ranger object is negleced because both a ranger object and a formula were passed.")
  }
  if(!is.null(ranger.obj))
  {
    if(sum(is.na(ranger.obj$variable.importance))!=0)
    {
      ranger.obj<-NULL
      warning("Warning: passed ranger object is negleced because it did not contain variable importance")
    }
  }
  if(sum(residual.model %in% c("Linear","RandomForest"))!=1)
  {
    stop("Error: residual.model must either be Linear or RandomForest")
  }
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
    ranger.obj<-ranger::ranger(formula=y~.,data=mf,importance = "permutation",...)
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


  RVIMP_obj<-lapply(Z,RVIMP_model,data=mf,orig.names=original.names,mod.type=residual.model,...=...)
  names(RVIMP_obj)<-Z
  if(keep.ranger)
  {
    RVIMP_obj<-list(call=sys.call(),variables=Z,residual.model=residual.model,ranger=ranger.obj,RVIMPs=RVIMP_obj,VIMP=ranger.obj$variable.importance)
  } else {
    RVIMP_obj<-list(call=sys.call(),variables=Z,residual.model=residual.model,ranger=NULL,RVIMPs=RVIMP_obj,VIMP=ranger.obj$variable.importance)
  }
  class(RVIMP_obj)<-c("RVIMP")
  RVIMP_obj
}
