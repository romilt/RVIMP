Bonferroni<-function(obj,alpha.new)
{
  rvimp<-obj$RVIMP
  vimp<-obj$VIMP
  confounder<-obj$name
  confounder_dens<-obj$dens_data
  quantile<-stats::quantile(confounder_dens,probs = 1-alpha.new)
  own_test<-quantile<vimp
  alpha.new<-round(alpha.new,4)

  data_dens<-data.frame(rvimps=confounder_dens)
  data_xintercept<-data.frame(stats=factor(c("quantil","vimp","rvimp"),levels=c("quantil","vimp","rvimp")),value=c(quantile,vimp,rvimp),linetype=factor(c("solid","solid","dashed"),levels=c("solid","dashed")))
  p<-ggplot2::ggplot()+ggplot2::geom_density(data_dens,mapping=ggplot2::aes(x=rvimps),colour="darkgreen",size=1)+
    ggplot2::geom_vline(data_xintercept,mapping=ggplot2::aes(xintercept=value,colour=stats,linetype=linetype),size=1)+
    ggplot2::ggtitle(paste("Multiple testresult for",confounder,"with Bonferroni procedure"))+ggplot2::ylab("Density")+
    ggplot2::xlab("RVIMPs")+ggplot2::scale_colour_manual(name="Statistics",labels=c(paste0(100*(1-alpha.new),"%-Quantile"), "VIMP","RVIMP"), values = c("red", "blue","orange"))+
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linetype = c(1, 1, 2))),linetype = FALSE)

  res<-list(name=confounder,alpha=alpha.new,test=own_test,p=obj$p,VIMP=vimp,RVIMP=rvimp,Quantil=quantile,plot=p)
}
