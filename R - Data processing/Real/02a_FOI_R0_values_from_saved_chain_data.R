#comp="C:/Users/Work_KJF82"
comp="C:/Users/kjfras16"
library(YEPaux)

{
  dir1a="Documents/00 - Big data files to back up infrequently/00 - YellowFeverDynamics key datasets"
  dir1b="Documents/0 - Yellow fever model MCMC results/Model runs 2023-10"
  dir2="Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2"
  
  input_data=readRDS(file=paste(comp,dir1a,"input_data_734_regions_burden.Rds",sep="/"))
  regions=input_data$region_labels
  n_regions=length(regions)
  
  enviro_data_all=read.csv(file=paste(comp,dir1a,"enviro_data_all2.csv",sep="/"),header=TRUE)
  enviro_glm=read.csv(file=paste(comp,dir1a,"enviro_glm_6covs.csv",sep="/"),header=TRUE)
  enviro_data=enviro_data_all[,colnames(enviro_data_all) %in% c("region",colnames(enviro_glm)[enviro_glm==TRUE])]
  enviro_data=enviro_data[enviro_data$region %in% regions,]
  enviro_data=enviro_data[order(enviro_data$region),]
  enviro_data$MIR.max=as.numeric(enviro_data$MIR.max)
}

chain_data_combined=readRDS(file=paste(comp,dir1b,dir2,"chain_data_combined.Rds",sep="/"))
n_entries=nrow(chain_data_combined)
chain_data_combined$m_FOI_Brazil=rep(1,nrow(chain_data_combined)) #Remove Brazil multiplier
FOI_R0_values=get_mcmc_FOI_R0_data(chain_data_combined[,c(2:ncol(chain_data_combined))],type="FOI+R0 enviro",enviro_data)

FOI_R0_summary=get_FOI_R0_dist_data(FOI_R0_values)

R0_target_values=c(0.5,0.7,1.0)
R0_probs=mcmc_R0_value_probs(FOI_R0_values,R0_target_values)

#saveRDS(FOI_R0_values,file=paste(comp,dir1b,dir2,"FOI_R0_datasets_734regions.Rds",sep="/"))
saveRDS(FOI_R0_summary,file=paste(comp,dir1b,dir2,"FOI_R0_summary_734regions_no_Brazil_multiplier.Rds",sep="/"))
saveRDS(R0_probs,file=paste(comp,dir1b,dir2,"R0_probs_734regions.Rds",sep="/"))
# 
# n_values=200
# interval=floor(n_entries/(n_values-1))
# rows=1+(interval*c(0:(n_values-1)))
# FOI_R0_values=get_mcmc_FOI_R0_data(chain_data_combined[rows,c(2:ncol(chain_data_combined))],type="FOI+R0 enviro",enviro_data)
# saveRDS(FOI_R0_values,paste(comp,"Documents/0 - R files/VIMC burden calculations - YEP + orderly/shared",
#                             "FOI_R0_200_datasets_734regions.Rds",sep="/"))
