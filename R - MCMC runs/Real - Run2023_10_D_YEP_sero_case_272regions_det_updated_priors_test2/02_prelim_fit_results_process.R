setwd("T:/Keith/Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2")
#setwd("T:/Keith/Run2023_10_A_YEP_sero_case_272regions_6cov_det_newpriors")

prelim_fit_results=readRDS(file="prelim_fit/prelim_fit_results02.Rds")

iterations_select=c(1:10)
for(i in iterations_select){
  if(i==iterations_select[1]){results_it_select=prelim_fit_results[[i]]}else{
    results_it_select=rbind(results_it_select,prelim_fit_results[[i]])
  }
}

ylabels=pretty(results_it_select$posterior,10)
matplot(x=c(1:nrow(results_it_select)),y=results_it_select$posterior,type="l",xlab="",ylab="posterior",yaxt="n",
        ylim=c(min(ylabels),max(ylabels)))
axis(side=2,at=ylabels,labels=ylabels)

n_params=16
param_names=colnames(results_it_select)[c(1:n_params)+1]
min_limits=max_limits=rep(NA,n_params)
for(i in c(1:n_params)){
  min_limits[i]=min(results_it_select[,i+1])
  max_limits[i]=max(results_it_select[,i+1])
}
ylimits=c(min(results_it_select$posterior)-1,max(results_it_select$posterior)+1)

par(mfrow=c(3,6),mar=c(2,1,2,1))
for(i in 1:n_params){
  matplot(x=c(log(min_limits[i])-1,log(max_limits[i])+1),y=ylimits,type="p",col=0,
          xlab="",ylab="",xaxt="n",yaxt="n")
  if(i<=12){
    xlabels=c(-25:0)
    axis(side=1,at=xlabels,labels=xlabels)
  } else {
    xlabels=c(0.05,0.1*c(1:10))
    axis(side=1,at=log(xlabels),labels=xlabels)
  }
  values=results_it_select[,colnames(results_it_select)==param_names[i]]  
  matplot(x=log(values),y=results_it_select$posterior,type="p",pch=16,cex=0.25,col=1,add=TRUE)
  title(main=param_names[i])
}
par(mfrow=c(1,1),mar=c(4,4,4,4))
