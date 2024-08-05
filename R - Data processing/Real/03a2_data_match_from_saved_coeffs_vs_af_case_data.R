comp="C:/Users/kjfras16"
#comp="C:/Users/Work_KJF82"
library("YEP")
library("YEPaux")

#Data
{
  dir1="Documents/0 - Yellow fever model MCMC results/Model runs 2023-10"
  dir2="Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2"
  dir3="Documents/00 - Big data files to back up infrequently/00 - YellowFeverDynamics key datasets"
  
  #Data
  obs_sero_data=NULL
  obs_case_data=read.csv(file=paste(comp,dir3,"case_data_af_adm1_2000_2020.csv",sep="/"),header=TRUE)
  
  input_data=readRDS(file=paste(comp,dir3,"input_data_734_regions_burden.Rds",sep="/"))
  input_data=input_data_process(input_data,obs_sero_data,obs_case_data)
  regions=input_data$region_labels
  
  enviro_data_all=read.csv(file=paste(comp,dir3,"enviro_data_all2.csv",sep="/"),header=TRUE)
  enviro_glm=read.csv(file=paste(comp,dir3,"enviro_glm_6covs.csv",sep="/"),header=TRUE)
  enviro_data=enviro_data_all[,colnames(enviro_data_all) %in% c("region",colnames(enviro_glm)[enviro_glm==TRUE])]
  enviro_data=enviro_data[enviro_data$region %in% regions,]
  enviro_data=enviro_data[order(enviro_data$region),]
  enviro_data$MIR.max=as.numeric(enviro_data$MIR.max)
  n_env_vars=ncol(enviro_data)-1
  
  type="FOI+R0 enviro"
  n_extra_params=4
  n_params=(2*n_env_vars)+n_extra_params
  columns=1+c(1:n_params)
}

#Get parameter values to test
chain_data=readRDS(file=paste(comp,dir1,dir2,"chain_data_combined.Rds",sep="/"))
n_values=1000
n_param_sets_all=nrow(chain_data)
interval=floor((n_param_sets_all-1)/(n_values-1))
rows=1+(interval*c(0:(n_values-1)))
assertthat::assert_that(max(rows)<=n_param_sets_all)
param_sets=chain_data[rows,columns]

cluster <- parallel::makeCluster(4)
const_list=list(type=type,n_reps=1,mode_start=1,dt=5.0,enviro_data=enviro_data,
                R0_fixed_values=NULL,vaccine_efficacy=NULL,p_rep_severe=NULL,
                p_rep_death=NULL,m_FOI_Brazil=NULL,p_severe_inf=0.12,p_death_severe_inf=0.39,
                deterministic=TRUE,mode_parallel="clusterMap",cluster=cluster)
model_data <- data_match_multi(param_sets,input_data,obs_sero_data,obs_case_data,const_list)
parallel::stopCluster(cluster)

saveRDS(model_data,file=paste(comp,dir1,dir2,"data_match/data_match1000_vs_af_data.Rds",sep="/"))