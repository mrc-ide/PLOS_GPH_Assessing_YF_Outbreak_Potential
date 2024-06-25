library(didehpc)
options(didehpc.cluster = "fi--didemrchnb")
setwd("T:/Keith/Run2022_10_G_IGL_new2")
didehpc_config(cores=1)

root <- "contexts"
packages <- c("YellowFeverDynamics")
sources <- c("R/run_function_multi.R")
package_sources <- conan::conan_sources(c("mrc-ide/odin.dust","mrc-ide/YellowFeverDynamics"))
ctx <- context::context_save(root,packages = packages, sources = sources,package_sources = package_sources)
obj <- didehpc::queue_didehpc(ctx)

{
  input_data=readRDS(file="exdata/input_data_IGL_new2.Rds")
  regions=input_data$region_labels
  
  obs_sero_data <- read.csv(file="exdata/obs_sero_data_IGL_new2_sparse.csv",header=TRUE)
  obs_case_data <- read.csv(file="exdata/obs_case_data_IGL_new2_sparse.csv",header=TRUE)
  obs_outbreak_data <- NULL
  
  type="FOI+R0 enviro"
  enviro_data=read.csv(file="exdata/enviro_data_IGL_new2.csv",header=TRUE)
  enviro_glm=read.csv(file="exdata/enviro_glm_IGL_new2.csv",header=TRUE)
  enviro_data_selected=enviro_data[,colnames(enviro_data) %in% c("region",colnames(enviro_glm)[enviro_glm==TRUE])]
  enviro_data_selected=enviro_data_selected[enviro_data_selected$region %in% regions,]
  n_env_vars=ncol(enviro_data_selected)-1
  env_vars=colnames(enviro_data_selected)[c(2:(n_env_vars+1))]  

  if(type %in% c("FOI+R0","FOI+R0 enviro")){R0_fixed_values=NULL} else {R0_fixed_values=rep(0,n_regions)}
}

{
  n_reps=5
  Niter=100001
  mode_start=1
  prior_type="norm"
  dt=5.0
  var_input_list=read.csv("exdata/IGL_new2_5_iterations_inputs_4_best_likelihoods_1_lowest_50sets.csv",header=FALSE)
  log_params_min=c(rep(-30,n_env_vars),rep(-5,n_env_vars),log(c(0.05,0.1)))
  log_params_max=c(rep(-10,n_env_vars),rep(0,n_env_vars),log(c(0.5,0.5)))
  vaccine_efficacy=1.0
  p_rep_severe=NULL
  p_rep_death=NULL
  m_FOI_Brazil=1.0
}

run <- obj$lapply(var_input_list,run_function_multi,input_data,obs_sero_data,obs_case_data,obs_outbreak_data,Niter,type,
                  log_params_min,log_params_max,n_reps,mode_start,prior_type,dt,enviro_data_selected,R0_fixed_values,
                  vaccine_efficacy,p_rep_severe,p_rep_death,m_FOI_Brazil)
run$wait(Inf)
