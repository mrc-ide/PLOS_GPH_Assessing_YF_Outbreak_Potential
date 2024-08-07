comp="C:/Users/kjfras16"
#comp="C:/Users/Work_KJF82"
library("YellowFeverDynamics")

#Data
{
  dir1a="OneDrive - Imperial College London/Cluster model runs data"
  dir1b="Documents/0 - Yellow fever model MCMC results/Model runs 2022-10"
  dir2="Run2022_10_H_IGL_new2_flatprior"
  dir3=c("A","B","C","D","E_lowlike")

  #Data
  obs_sero_data=read.csv(file=paste(comp,dir1b,dir2,"exdata/obs_sero_data_IGL_new2_sparse.csv",sep="/"),header=TRUE)
  obs_case_data=read.csv(file=paste(comp,dir1b,dir2,"exdata/obs_case_data_IGL_new2_sparse.csv",sep="/"),header=TRUE)
  obs_outbreak_data=NULL

  input_data=readRDS(file=paste(comp,dir1b,dir2,"exdata/input_data_IGL_new2.Rds",sep="/"))
  input_data=input_data_process(input_data,obs_sero_data,obs_case_data,obs_outbreak_data)
  regions=input_data$region_labels
  enviro_data=read.csv(file=paste(comp,dir1b,dir2,"exdata/enviro_data_IGL_new2.csv",sep="/"),header=TRUE)
  
  type="FOI+R0 enviro"
  n_env_vars=ncol(enviro_data)-1
  n_extra_params=2 #p_rep_severe, p_rep_death
  n_params=(2*n_env_vars)+n_extra_params
  columns=1+c(1:n_params)
}

#Get parameter values to test
chain_data=readRDS(file=paste(comp,dir1b,dir2,"chain_data_combined.Rds",sep="/"))
n_values=1000
n_param_sets_all=nrow(chain_data)
interval=floor(n_param_sets_all/(n_values-1))
rows=1+(interval*c(0:(n_values-1)))
param_sets=chain_data[rows,columns]

const_list=list(type=type,n_reps=10,mode_start=1,dt=5.0,enviro_data=enviro_data,
                R0_fixed_values=NULL,vaccine_efficacy=1.0,p_rep_severe=NULL,
                p_rep_death=NULL,m_FOI_Brazil=1.0)

model_data <- data_match_multi(param_sets,input_data,obs_sero_data,obs_case_data,obs_outbreak_data,const_list)
saveRDS(model_data,file=paste(comp,dir1b,dir2,"data_match_1000values.Rds",sep="/"))

plot_type="all"
sero_graphs <- sero_match_graphs(model_data,obs_sero_data,plot_type=plot_type,text_size=14)
png(filename=paste(comp,dir1b,dir2,"images/data_match_1000values_sero.png",sep="/"),width=2880,height=1440)
Rmisc::multiplot(plotlist=sero_graphs,cols=floor(sqrt(length(sero_graphs))))
dev.off()

case_graphs <- case_match_graphs(model_data,obs_case_data,input_data,plot_type=plot_type,text_size=12)
png(filename=paste(comp,dir1b,dir2,"images/data_match_1000values_cases.png",sep="/"),width=2160,height=1080)
Rmisc::multiplot(plotlist=case_graphs$cases_graphs,cols=floor(sqrt(length(case_graphs$cases_graphs))))
dev.off()
png(filename=paste(comp,dir1b,dir2,"images/data_match_1000values_deaths.png",sep="/"),width=2160,height=1080)
Rmisc::multiplot(plotlist=case_graphs$deaths_graphs,cols=floor(sqrt(length(case_graphs$deaths_graphs))))
dev.off()