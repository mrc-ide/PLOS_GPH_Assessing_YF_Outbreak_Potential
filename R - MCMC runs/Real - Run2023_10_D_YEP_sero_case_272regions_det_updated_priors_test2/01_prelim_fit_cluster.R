library(didehpc)
setwd("T:/Keith/Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2")

root <- "contexts"
packages <- c("YEP","odin.dust")
sources <- c("R/cluster_functions.R")
package_sources <- conan::conan_sources(c("mrc-ide/odin.dust","mrc-ide/YEP"))
ctx <- context::context_save(root,packages = packages, sources = sources,package_sources = package_sources)
obj <- queue_didehpc(ctx,config=didehpc_config(credentials=list(username="kjfras16",password="Wednesday123!"),
                                               cluster="fi--dideclusthn",parallel=FALSE,cores=8),provision="upgrade")

#Data
{
  input_data=readRDS(file="exdata/input_data_272regions.Rds")
  input_data$vacc_data[is.na(input_data$vacc_data)]=0.0

  obs_sero_data <- read.csv(file="exdata/sero_data_af.csv",header=TRUE)
  obs_sero_data$vc_factor[is.na(obs_sero_data$vc_factor)]=0.7
  obs_case_data <- read.csv(file="exdata/case_data_la_omit_COL26_3_6countries.csv",
                            header=TRUE)
  
  enviro_data=read.csv(file="exdata/enviro_data_272regions_6cov.csv",header=TRUE)
  n_env_vars=ncol(enviro_data)-1
  env_vars=colnames(enviro_data)[c(2:(n_env_vars+1))]
}

{ #NB - Save settings in text file
  n_iterations=10
  n_param_sets=100
  n_bounds=10
  type="FOI+R0 enviro"
  log_params_min=c(rep(-25,n_env_vars),rep(-10,n_env_vars),log(c(0.95,5e-2,5e-2,1e-2)))
  log_params_max=c(rep(-10,n_env_vars),rep(0,n_env_vars),log(c(1.0,1.0,1.0,1.0)))
  mode_start=3
  prior_settings=list(type="norm",
                      norm_params_mean=c(rep(0,n_env_vars),c(0,0,0,0,-7,0),0.975,rep(0.1,3)),
                      norm_params_sd=c(rep(30,n_env_vars),rep(5,n_env_vars),rep(0.05,4)),
                      R0_mean=4.8,R0_sd=1.0)
  dt=5.0
  n_reps=1
  R0_fixed_values=NULL
  p_severe_inf = 0.12
  p_death_severe_inf = 0.39
  add_values=list(vaccine_efficacy=NULL,p_rep_severe=NULL,p_rep_death=NULL,m_FOI_Brazil=NULL)
  deterministic=TRUE
  mode_parallel="clusterMap"
  n_cores=8
  filename="prelim_fit/prelim_fit_results02.Rds"
}
var_input_list=c(4) #Seed value(s)

run <- obj$lapply(var_input_list,run_prelim_fit,n_iterations,n_param_sets,n_bounds,type,log_params_min,log_params_max,
                  input_data,obs_sero_data,obs_case_data,mode_start,prior_settings,dt,n_reps,enviro_data,
                  R0_fixed_values,p_severe_inf,p_death_severe_inf,add_values,deterministic,mode_parallel,n_cores,filename)
run$wait(Inf)


