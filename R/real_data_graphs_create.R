# TODO - Adjust x values on posterior graph
comp="C:/Users/kjfras16"
library(YEPaux) #Need to fix get_mcmc_enviro_coeff_multichain and subordinate function
library(ggplot2)
library(Rmisc)
#Set folders to source environmental data and raw MCMC output
{
  dir1="Documents/00 - Big data files to back up infrequently/00 - YellowFeverDynamics key datasets"
  dir2a="OneDrive - Imperial College London/Cluster model runs data/Complete"
  dir2b="Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2/outputs"
  dir2c=c("A","B","C","D")

  input_data=readRDS(file=paste(comp,dir1,"input_data_272regions_case_sero.Rds",sep="/"))
  regions=input_data$region_labels
  n_regions=length(regions)

  enviro_filename="enviro_data_all2.csv"
  enviro_data=read.csv(file=paste(comp,dir1,enviro_filename,sep="/"),header=TRUE)
  enviro_glm=read.csv(file=paste(comp,dir1,"enviro_glm_6covs.csv",sep="/"),header=TRUE)
  enviro_data_selected=enviro_data[,colnames(enviro_data) %in% c("region",colnames(enviro_glm)[enviro_glm==TRUE])]
  enviro_data_selected=subset(enviro_data_selected,region %in% regions)
  enviro_data_selected$MIR.max=as.numeric(enviro_data_selected$MIR.max)
  prob_names=c("vaccine_efficacy","p_rep_severe","p_rep_death","m_FOI_Brazil")
  n_params=(2*(ncol(enviro_data_selected)-1))+length(prob_names)
}

#Load data from all folders
folder_list=rep(NA,4)
for(n2 in c(1:4)){folder_list[n2]=paste(comp,dir2a,dir2b,dir2c[n2],sep="/")}
input_frames=get_mcmc_datasets_multichain(folder_list)

#Get and plot likelihood values and calculate Gelman factor for selected folders
selection=c(1,2,3,4)
chain_data1=display_multichain_progress(input_frames,selection,burnin_values=NULL,end_values=NULL)

#Set burnin and/or end values and plot likelihood values for selected portions of chains
burnin_values=rep(60000,4)
end_values=NULL
chain_data2=display_multichain_progress(input_frames,selection,burnin_values,end_values)

#Adjust covariate names
colnames(enviro_data_selected)[7]="NHP_combined"

#Make datasets for each chain
coeff_datasets=get_mcmc_enviro_coeff_multichain(input_frames,selection,burnin_values,NULL,"FOI+R0 enviro",enviro_data_selected)
prob_datasets=get_mcmc_additional_params_multichain(input_frames,selection,burnin_values,NULL,prob_names)

#Generate graphs and generate datasets combining chains
for(i in 1:length(selection)){
  if(i==1){
    coeff_datasets_all=coeff_datasets[[i]]
    prob_datasets_all=prob_datasets[[i]]
  } else {
    coeff_datasets_all=rbind(coeff_datasets[[i]])
    prob_datasets_all=rbind(prob_datasets[[i]])
  }
}
colnames(prob_datasets_all)[1]="vacc_eff"
colnames(prob_datasets_all)[4]="F_Brazil"

#Create graphs
plot_type="violin"
G1_all <-plot_mcmc_enviro_coeff_data(coeff_datasets_all,names(table(coeff_datasets_all$env_var)),plot_type,
                                          text_size1=30)
G2_all <- plot_mcmc_prob_data(prob_datasets_all,plot_type,colnames(prob_datasets_all),text_size1=30)

#Print graphs without y-axis limits
{
  png(filename="images/Real - SI/real_FOI_coeffs_fit_no_limits.png",width=1440,height=720)
  print(G1_all$p_FOI)
  dev.off()
  png(filename="images/Real - SI/real_R0_coeffs_fit_no_limits.png",width=1440,height=720)
  print(G1_all$p_R0)
  dev.off()
}

#Add y-axis limits
FOI_limits=c(log(1e-12,10),ceiling(log(max(coeff_datasets_all$FOI_coeffs),10)))
FOI_labels=10^c(FOI_limits[1]:FOI_limits[2])
G1_all$p_FOI <- G1_all$p_FOI + scale_y_continuous(name="FOI coefficients",
                                                  breaks=log(FOI_labels,10),labels=FOI_labels,limits=FOI_limits)
R0_limits=c(log(1e-5,10),ceiling(log(max(coeff_datasets_all$R0_coeffs),10)))
R0_labels=10^c(R0_limits[1]:R0_limits[2])
G1_all$p_R0 <- G1_all$p_R0 + scale_y_continuous(name="R0 coefficients",breaks=log(R0_labels,10),labels=R0_labels,
                                                limits=R0_limits)

#Set iteration numbers correctly to take into account missing values
it_values1=rep(c(1:9990),10)+sort(rep(c(0:9)*10000,9990))
it_values2=it_values1[c(60000:length(it_values1))]

#Print graphs with limits
{
  png(filename="images/Real - SI/real_4chains.png",width=1440,height=720)
  par(mar=c(5,5,1,1))
  matplot(x=c(0,100000),y=c(-4700,-4238),type="p",col=0,xlab="Iteration",ylab="Log posterior probability",cex.lab=2.0,cex.axis=2.0)
  for(n in selection){
    #matplot(x=c(1:length(chain_data1$like_values[[n]])),y=chain_data1$like_values[[n]],type="l",col=n,add=TRUE)
    matplot(x=it_values1,y=chain_data1$like_values[[n]],type="l",col=n,add=TRUE)
  }
  dev.off()
  png(filename="images/Real - SI/real_4chains_post_burnin.png",width=1440,height=720)
  par(mar=c(5,5,1,1))
  matplot(x=c(60000,100000),y=c(-4270,-4238),type="p",col=0,xlab="Iteration",ylab="Log posterior probability",
          cex.lab=2.0,cex.axis=2.0)
  for(n in selection){
    #matplot(x=59999+c(1:length(chain_data2$like_values[[n]])),y=chain_data2$like_values[[n]],type="l",col=n,add=TRUE)
    matplot(x=it_values2,y=chain_data2$like_values[[n]],type="l",col=n,add=TRUE)
  }
  dev.off()
  png(filename="images/Real - SI/real_FOI_coeffs_fit.png",width=1680,height=600)
  print(G1_all$p_FOI)
  dev.off()
  png(filename="images/Real - SI/real_R0_coeffs_fit.png",width=1680,height=600)
  print(G1_all$p_R0)
  dev.off()
  png(filename="images/Real - SI/real_report_probs_fit.png",width=1680,height=600)
  print(G2_all)
  dev.off()
}
