#' Generate an estimation for RVIMPs distribution based on resamplings
#' @description Estimation of the RVIMP-distribution based on resamplings
#' @param data data set the estimation shall be based on
#' @param variables Variables the distribution shall be estimated for
#' @param residual Residuals if they are precalculated.
#' @param rep Number of resamplings the distribution shall be estimated on
#' @param sample.seed Seed for drawing the resamplings
#' @param residual.model Model the residuals of \code{variables} shall be estimated with. Either "Linear" or "RandomForest".
#' @param num.trees Number of trees within each ranger object.


get_distribution<-function(data,variables,residual,rep,sample.seed,residual.model,num.trees){
  data.frame(matrix(sapply(1:rep,function(x,dat=data){
    set.seed(sample.seed*x)
    in_bag<-sample(1:dim(dat)[1],ceiling((1-1/exp(1))*dim(dat)[1]),F)
    sample_dat<-dat[in_bag,]
    if(residual.model=="Precalculated")
    {
      residual<-residual[in_bag]
    }
    org_rvimps<-sapply(variables,function(i){
      conf.index<-which(colnames(sample_dat)==i)
      colnames(sample_dat)[conf.index]<-"z"
      if(residual.model=="Precalculated"){
        sample_dat[,conf.index]<-residual
      } else if(residual.model=="Linear")
        {
          # mod<-stats::lm(z~.-y,data=sample_dat)
          # mod_sum<-summary(mod)
          # sig_names<-names(which(mod_sum$coefficients[,4]<0.05))
          sig_names<-names(which(summary(stats::lm(z~.-y,data=sample_dat))$coefficients[,4]<0.05))
          if("(Intercept)" %in% sig_names)
          {
            sig_names<-sig_names[-1]
            if(identical(sig_names, character(0)) ) {
              sample_dat[,conf.index]<-sample_dat[,conf.index]
            } else {
              mod<-stats::lm(stats::as.formula(paste("z ~ ", paste(sig_names, collapse= "+"))),data=sample_dat)
              sample_dat[,conf.index]<-scale(mod$residuals)
            }
          } else if(identical(sig_names, character(0)) ) {
            sample_dat[,conf.index]<-sample_dat[,conf.index]
          } else {
            mod<-stats::lm(stats::as.formula(paste("z ~ ", paste(sig_names, collapse= "+"))),data=sample_dat)
            sample_dat[,conf.index]<-scale(mod$residuals)
          }
        } else {
          mod<-ranger::ranger(z~.-y,data=sample_dat)
          sample_dat[,conf.index]<-scale(sample_dat[,conf.index]-mod$predictions)
        }
      conf_rvimp<-ranger::ranger(y~.,data=sample_dat,importance="permutation",write.forest = F,seed=sample.seed*x*conf.index,num.trees = num.trees)$variable.importance["z"]
      conf_rvimp
    })
    org_rvimps
  }),nrow=length(variables)))
}

