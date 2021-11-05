RVIMP_model<-function(confounder,data,orig.names,mod.type,...){

  conf.index<-which(orig.names==confounder)
  colnames(data)[conf.index]<-"z"

  if(mod.type=="Linear")
  {
    mod<-stats::lm(z~.-y,data=data)
    mod_sum<-summary(mod)
    sig_names<-names(which(mod_sum$coefficients[,4]<0.05))
    if("(Intercept)" %in% sig_names)
    {
      sig_names<-sig_names[-1]
      if(identical(sig_names, character(0)) ) {
        data[,conf.index]<-data[,conf.index]
      } else {
        mod<-stats::lm(stats::as.formula(paste("z ~ ", paste(sig_names, collapse= "+"))),data=data)
        data[,conf.index]<-scale(stats::residuals(mod))
      }
    } else if(identical(sig_names, character(0)) ) {
      data[,conf.index]<-data[,conf.index]
    } else {
      mod<-stats::lm(stats::as.formula(paste("z ~ ", paste(sig_names, collapse= "+"))),data=data)
      data[,conf.index]<-scale(stats::residuals(mod))
    }
  } else {
    mod<-ranger::ranger(z~.-y,data=data)
    data[,conf.index]<-scale(data[,conf.index]-mod$predictions)
  }
  rvimp_full_resid_forest<-ranger::ranger(y~.,data = data,importance = "permutation",...)
  res<-list(conf=confounder,model=rvimp_full_resid_forest,RVIMP=rvimp_full_resid_forest$variable.importance["z"])
  class(res)<-"RVIMP.model"
  res
}
