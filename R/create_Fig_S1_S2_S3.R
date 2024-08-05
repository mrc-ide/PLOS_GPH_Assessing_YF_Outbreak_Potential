#comp="C:/Users/Work_KJF82"
comp="C:/Users/kjfras16"

library(ggplot2)
library(Rmisc)
library(YellowFeverDynamics)

#Set folders to source environmental data and raw MCMC output
{
  dir1a="OneDrive - Imperial College London/Cluster model runs data/Complete"
  dir1b="Documents/0 - Publications+Reports/Paper 05 - Yellow fever outbreak dynamics model"
  dir2="Run2022_10_G_IGL_new2"
  dir3=c("A","B","C","D","E_lowlike")
  
  input_data=readRDS(file=paste(comp,dir1b,"data/input_data_IGL_new2.Rds",sep="/"))
  regions=input_data$region_labels
  n_regions=length(regions)
  
  enviro_data=read.csv(file=paste(comp,dir1b,"data/enviro_data_IGL_new2.csv",sep="/"),header=TRUE)
  enviro_glm=read.csv(file=paste(comp,dir1b,"data/enviro_glm_IGL_new2.csv",sep="/"),header=TRUE)
  enviro_data=enviro_data[,colnames(enviro_data) %in% c("region",colnames(enviro_glm)[enviro_glm==TRUE])]
  enviro_data=enviro_data[enviro_data$region %in% regions,]
  enviro_data=enviro_data[order(enviro_data$region),]
  
  prob_names=c("p_rep_severe","p_rep_death")
  n_params=(2*(ncol(enviro_data)-1))+length(prob_names)
  type="FOI+R0 enviro"
}

#Load data from all folders
folder_list=rep(NA,5)
for(n2 in c(1:5)){folder_list[n2]=paste(comp,dir1a,dir2,dir3[n2],sep="/")}
input_frames=get_mcmc_datasets_multichain(folder_list)

#Get and plot likelihood values and calculate Gelman factor for selected folders
selection=c(1:4)
chain_data1=display_multichain_progress(input_frames,selection,burnin_values=NULL,end_values=NULL)

#Set burnin and/or end values and plot likelihood values for selected portions of chains
burnin_values=rep(50000,4)
end_values=NULL
chain_data2=display_multichain_progress(input_frames,selection,burnin_values,end_values)

#Make datasets
for(n2 in selection){
  n=match(n2,selection)
  if(n2==selection[1]){chain_data_combined=input_frames[[n2]][c(burnin_values[n]:nrow(input_frames[[n2]])),]} else {
    chain_data_combined=rbind(chain_data_combined,input_frames[[n2]][c(burnin_values[n]:nrow(input_frames[[n2]])),])
  }
}
saveRDS(chain_data_combined,file=paste(comp,dir1b,"data/chain_data_combined.Rds",sep="/"))
coeff_datasets=get_mcmc_enviro_coeff_multichain(input_frames,selection,burnin_values,NULL,type,enviro_data)
FOI_R0_datasets=get_mcmc_FOI_R0_multichain(input_frames,selection,burnin_values,NULL,type,enviro_data)
prob_datasets=get_mcmc_probs_multichain(input_frames,selection,burnin_values,NULL,prob_names)

#Import correct values
FOI_R0_real=read.csv(paste(comp,dir1b,"data/FOI_R0_data_IGL_new2.csv",sep="/"),header=TRUE)
FOI_R0_real=cbind(FOI_R0_real,c(1:n_regions))
p_real=data.frame(name=c("p_rep_severe","p_rep_death"),value=c(0.1,0.2))
c_real=read.csv(paste(comp,dir1b,"data/enviro_coeffs_IGL_new2.csv",sep="/"),header=TRUE)
n_env_vars=5
c_real2=data.frame(FOI=log10(exp(as.numeric(c_real[c(1:n_env_vars)]))),
                                 R0=log10(exp(as.numeric(c_real[c(1:n_env_vars)+n_env_vars]))))

#Generate datasets combining chains
plot_type="violin"
for(i in 1:length(selection)){
  if(i==1){
    coeff_datasets_all=coeff_datasets[[i]]
    FOI_R0_datasets_all=FOI_R0_datasets[[i]]
    prob_datasets_all=prob_datasets[[i]]
  } else {
    coeff_datasets_all=rbind(coeff_datasets_all,coeff_datasets[[i]])
    FOI_R0_datasets_all=rbind(FOI_R0_datasets_all,FOI_R0_datasets[[i]])
    prob_datasets_all=rbind(prob_datasets_all,prob_datasets[[i]])
  }
}

#Generate graphs combining chains
{
  G1_all <-plot_mcmc_enviro_coeff_data(coeff_datasets_all,names(table(coeff_datasets_all$env_var)),plot_type,20)
  G1_all$p_FOI <- G1_all$p_FOI+geom_line(data=c_real2,aes(x=c(1:n_env_vars),y=FOI,colour="red"))+guides(colour="none")
  G1_all$p_FOI <- G1_all$p_FOI+geom_point(data=c_real2,aes(x=c(1:n_env_vars),y=FOI,colour="red",size=3))
  G1_all$p_R0 <- G1_all$p_R0+geom_line(data=c_real2,aes(x=c(1:n_env_vars),y=R0,colour="red"))+guides(colour="none")
  G1_all$p_R0 <- G1_all$p_R0+geom_point(data=c_real2,aes(x=c(1:n_env_vars),y=R0,colour="red",size=3))
  
  FOI_limits=c(log(1e-10,10),ceiling(log(max(coeff_datasets_all$FOI_coeffs),10)))
  FOI_labels=10^c(FOI_limits[1]:FOI_limits[2])
  G1_all$p_FOI <- G1_all$p_FOI + scale_y_continuous(name="FOI coefficients",breaks=log(FOI_labels,10),labels=FOI_labels,
                                      limits=FOI_limits)
  R0_limits=c(log(1e-5,10),ceiling(log(max(coeff_datasets_all$R0_coeffs),10)))
  R0_labels=10^c(R0_limits[1]:R0_limits[2])
  G1_all$p_R0 <- G1_all$p_R0 + scale_y_continuous(name="R0 coefficients",breaks=log(R0_labels,10),labels=R0_labels,
                                    limits=R0_limits)
  
  G2_all <-plot_mcmc_FOI_R0_data(FOI_R0_datasets_all,enviro_data$region,plot_type,20)
  G2_all$p_FOI <- G2_all$p_FOI+geom_line(data=FOI_R0_real,aes(x=c(1:n_regions),y=log(FOI),colour="red"))+guides(colour="none")
  G2_all$p_FOI <- G2_all$p_FOI+geom_point(data=FOI_R0_real,aes(x=c(1:n_regions),y=log(FOI),colour="red",size=3))
  G2_all$p_R0 <- G2_all$p_R0+geom_line(data=FOI_R0_real,aes(x=c(1:n_regions),y=R0,colour="red"))+guides(colour="none")
  G2_all$p_R0 <- G2_all$p_R0+geom_point(data=FOI_R0_real,aes(x=c(1:n_regions),y=R0,colour="red",size=3))
  
  G3_all <- plot_mcmc_prob_data(prob_datasets_all,plot_type,prob_names,20)
  G3_all <- G3_all+geom_line(data=p_real,aes(x=c(1:length(prob_names)),y=log(value),colour="red"))+guides(colour="none")
  G3_all <- G3_all+geom_point(data=p_real,aes(x=c(1:length(prob_names)),y=log(value),colour="red",size=3))
}

#Print graphs
{
  #Set iteration numbers correctly to take into account missing values
  it_values1=rep(c(1:9990),10)+sort(rep(c(0:9)*10000,9990))
  
  png(filename="images/IGL - SI/IGL_4chains_new.png",width=1440,height=840)
  par(mar=c(5,5,1,1))
  matplot(x=c(1,max(end_values)),y=c(like_min,like_max),type="p",col=0,xlab="Iteration",ylab="Log posterior probability",cex.axis=2.0,cex.lab=2.0)
  for(i in 1:length(datasets_selected)){
    n_data=datasets_selected[i]
    matplot(x=it_values1,y=like_values[[n_data]],type="l",col=n_data,add=TRUE)
  }
  dev.off()
  png(filename="IGL_FOI_coeffs_fit.png",width=1440,height=720)
  print(G1_all$p_FOI)
  dev.off()
  png(filename="IGL_R0_coeffs_fit.png",width=1440,height=720)
  print(G1_all$p_R0)
  dev.off()
  png(filename="IGL_FOI_fit.png",width=1440,height=720)
  print(G2_all$p_FOI)
  dev.off()
  png(filename="IGL_R0_fit.png",width=1440,height=720)
  print(G2_all$p_R0)
  dev.off()
  png(filename="IGL_report_probs_fit.png",width=1440,height=720)
  print(G3_all)
  dev.off()
}