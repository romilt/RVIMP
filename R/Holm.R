Holm<-function(name,alpha,obj)
{
  rvimp<-obj[[name]]$RVIMP
  vimp<-obj[[name]]$VIMP
  confounder<-name
  confounder_dens<-obj[[name]]$dens_data
  quantile<-stats::quantile(confounder_dens,probs = 1-alpha)
  quantile_low<-stats::quantile(confounder_dens,probs = alpha)
  own_test<-quantile<vimp
  own_test_low<-quantile_low>0
  alpha<-round(alpha,4)


  data_dens<-data.frame(rvimps=confounder_dens)
  data_xintercept<-data.frame(stats=factor(c("quantil","vimp","rvimp"),levels=c("quantil","vimp","rvimp")),value=c(quantile,vimp,rvimp),linetype=factor(c("solid","solid","dashed"),levels=c("solid","dashed")))
  p<-ggplot2::ggplot()+ggplot2::geom_density(data_dens,mapping=ggplot2::aes(x=rvimps),colour="darkgreen",size=1)+
    ggplot2::geom_vline(data_xintercept,mapping=ggplot2::aes(xintercept=value,colour=stats,linetype=linetype),size=1)+
    ggplot2::ggtitle(paste("Multiple test result for",confounder,"with Holm procedure"))+ggplot2::ylab("Density")+
    ggplot2::xlab("RVIMPs")+ggplot2::scale_colour_manual(name="Statistics",labels=c(paste0(100*(1-alpha),"%-Quantile"), "VIMP","RVIMP"), values = c("red", "blue","orange"))+
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linetype = c(1, 1, 2))),linetype = FALSE)

  res<-list(name=confounder,alpha=alpha,test=own_test,p=obj[[name]]$p_dep,test_inf=own_test_low,p_inf=obj[[name]]$p_inf,VIMP=vimp,RVIMP=rvimp,Quantil=quantile,plot=p)
}
