# TODO - Adjust x values on posterior graph
comp="C:/Users/kjfras16"

library(ggplot2)
library(Rmisc)
library(YEPaux)
library(coda)

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
datasets=input_frames
datasets_selected=c(1:4)
burnin_values=rep(1,length(datasets_selected))
for(i in 1:length(datasets_selected)){
  end_values[i]=length(datasets[[datasets_selected[i]]]$posterior_current)
}

rows=like_values=list()
itmax=0
like_min=0
like_max=-10000
mcmc_list=mcmc.list()
for(i in 1:length(datasets_selected)){
  n_data=datasets_selected[i]
  rows[[i]]=c(burnin_values[i]:end_values[i])
  input_frame=datasets[[n_data]]
  if("flag_accept" %in% colnames(input_frame)){param_names=get_mcmc_params(input_frame)} else {
    param_names=colnames(input_frame)[c(2:ncol(input_frame))]}
  columns=which(colnames(input_frame) %in% param_names)
  
  mcmc_list[[i]]=mcmc(data=input_frame[,columns],start=burnin_values[i],end=end_values[i],thin=1)
  like_values[[n_data]]=input_frame$posterior_current[rows[[i]]]
  like_values[[n_data]][is.infinite(like_values[[n_data]])]=-10000
  like_min=min(like_min,min(like_values[[n_data]]))
  like_max=max(like_max,max(like_values[[n_data]]))
}

png(filename="images/IGL - SI/IGL_4chains_new.png",width=1440,height=840)
par(mar=c(5,5,1,1))
matplot(x=c(1,max(end_values)),y=c(like_min,like_max),type="p",col=0,xlab="Iteration",ylab="Log posterior probability",cex.axis=2.0,cex.lab=2.0)
for(i in 1:length(datasets_selected)){
  n_data=datasets_selected[i]
  matplot(x=rows[[i]],y=like_values[[n_data]],type="l",col=n_data,add=TRUE)
}
dev.off()