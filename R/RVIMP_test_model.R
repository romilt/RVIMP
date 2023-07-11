RVIMP_test_model<-function(variables,data,dens_data,org.names,mod.type,alpha,residual,rang.obj,...){
  conf.index<-which(org.names==variables)
  colnames(data)[conf.index]<-"z"
  if(mod.type=="Precalculated"){
    data[,conf.index]<-residual
  } else if(mod.type=="Linear")
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

  rvimp<-ranger::ranger(y~.,data = data,importance = "permutation",...)$variable.importance["z"]
  vimp<-rang.obj$variable.importance[variables]
  confounder_dens<-as.numeric(dens_data[variables,])
  confounder_dens<-confounder_dens+(rvimp-mean(confounder_dens))
  quantile<-stats::quantile(confounder_dens,probs = 1-alpha)
  quantile_low<-stats::quantile(confounder_dens,probs = alpha)
  p_value<-mean(confounder_dens>vimp)
  p_value_low<-mean(confounder_dens<0)
  own_test<-quantile<vimp
  own_test_0<-quantile_low>0


  data_dens<-data.frame(rvimps=confounder_dens)
  data_xintercept<-data.frame(stats=factor(c("quantil","vimp","rvimp"),levels=c("quantil","vimp","rvimp")),value=c(quantile,vimp,rvimp),linetype=factor(c("solid","solid","dashed"),levels=c("solid","dashed")))
  p<-ggplot2::ggplot()+ggplot2::geom_density(data_dens,mapping=ggplot2::aes(x=rvimps),colour="darkgreen",size=1)+
    ggplot2::geom_vline(data_xintercept,mapping=ggplot2::aes(xintercept=value,colour=stats,linetype=linetype),size=1)+
    ggplot2::ggtitle(paste("Test result for",variables,"as variable of interest"))+ggplot2::ylab("Density")+
    ggplot2::xlab("RVIMPs")+ggplot2::scale_colour_manual(name="Statistics",labels=c(paste0(100*(1-alpha),"%-Quantile"), "VIMP","RVIMP"), values = c("red", "blue","orange"))+
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linetype = c(1, 1, 2))),linetype = FALSE)

  res<-list(name=variables,test_dep=own_test,p_dep=p_value,test_inf=own_test_0,p_inf=p_value_low,VIMP=vimp,RVIMP=rvimp,Quantil=quantile,plot=p,dens_data=confounder_dens)
  return(res)
}
