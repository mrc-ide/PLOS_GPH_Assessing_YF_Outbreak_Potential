#--------------------------------------------------------------------------------------------------------------------
#Running mcmc_prelim_fit
hpc_prelim_fit <- function(var_input=c(), n_iterations=1,n_param_sets=1,n_bounds=1,
                           log_params_min=c(),log_params_max=c(),
                           input_data_file="",obs_sero_data_file="",obs_case_data_file="",
                           mode_start=1,prior_settings=list(),dt=1.0,n_reps=1,enviro_data_file="",
                           p_severe_inf=1.0,p_death_severe_inf=1.0,add_values=list(),
                           deterministic=FALSE,mode_parallel="none",n_cores=1,filename=""){

  seed=var_input[1]

  input_data=readRDS(file=input_data_file)
  input_data$vacc_data[is.na(input_data$vacc_data)]=0.0

  obs_sero_data <- read.csv(file=obs_sero_data_file,header=TRUE)
  obs_sero_data$vc_factor[is.na(obs_sero_data$vc_factor)]=0.7
  obs_case_data <- read.csv(file=obs_case_data_file,header=TRUE)

  enviro_data=read.csv(file=enviro_data_file,header=TRUE)

  if(mode_parallel=="clusterMap"){cluster=parallel::makeCluster(n_cores)}else{cluster=NULL}
  set.seed(seed)
  prelim_fit_results <- YEP::mcmc_prelim_fit(n_iterations,n_param_sets,n_bounds,log_params_min,log_params_max,
                                             input_data,obs_sero_data,obs_case_data,mode_start,prior_settings,dt,n_reps,
                                             enviro_data,p_severe_inf,p_death_severe_inf,add_values,
                                             deterministic,mode_parallel,cluster)
  if(is.null(cluster)==FALSE){parallel::stopCluster(cluster)}

  saveRDS(prelim_fit_results,file=filename)

  return(prelim_fit_results)
}
#--------------------------------------------------------------------------------------------------------------------
#Running mcmc
hpc_mcmc <- function(var_input=c(),input_data_file = "",obs_sero_data_file = "",obs_case_data_file = "",
                     Niter = 1, mode_start = 0, prior_settings=list(type="zero"),
                     dt = 1,n_reps = 1,enviro_data_file = "", p_severe_inf = 0.12,p_death_severe_inf = 0.39,
                     add_values=list(vaccine_efficacy = NA, p_rep_severe = NA,p_rep_death = NA,
                                     m_FOI_Brazil = 1),
                     deterministic = FALSE,mode_parallel = "none",cores=NULL){

  seed=as.numeric(var_input[1])
  filename_prefix=as.character(var_input[2])
  log_params_ini=log(as.numeric(var_input[c(3:length(var_input))]))

  input_data=readRDS(file=input_data_file)
  input_data$vacc_data[is.na(input_data$vacc_data)]=0.0

  obs_sero_data <- read.csv(file=obs_sero_data_file,header=TRUE)
  obs_sero_data$vc_factor[is.na(obs_sero_data$vc_factor)]=0.7
  obs_case_data <- read.csv(file=obs_case_data_file,header=TRUE)

  enviro_data=read.csv(file=enviro_data_file,header=TRUE)

  input_data=YEP::input_data_process(input_data,obs_sero_data,obs_case_data)

  if(mode_parallel){cluster <- parallel::makeCluster(cores)} else {cluster=NULL}

  set.seed(seed) #For reproducibility
  output <- YEP::MCMC(log_params_ini,input_data,obs_sero_data,obs_case_data,filename_prefix,
                      Niter,mode_start,prior_settings,dt,n_reps,enviro_data,p_severe_inf,p_death_severe_inf,
                      add_values,deterministic,mode_parallel,cluster)

  if(mode_parallel){parallel::stopCluster(cluster)}

  return(output)
}
#--------------------------------------------------------------------------------------------------------------------
#Running mcmc with multiple chains
hpc_mcmc_multi <- function(input_data_file = "",obs_sero_data_file = "",obs_case_data_file = "",
                               Niter = 1,mode_start = 0, prior_settings=list(type="zero"),
                               dt = 1,n_reps = 1,enviro_data_file = "",p_severe_inf = 0.12,
                               p_death_severe_inf = 0.39,
                               add_values=list(vaccine_efficacy = NA, p_rep_severe = NA,p_rep_death = NA,
                                               m_FOI_Brazil = 1),
                               deterministic = FALSE,mode_parallel = FALSE,cores=NULL){

  seed=as.numeric(A)
  filename_prefix=as.character(B)
  log_params_ini=log(as.numeric(c(C,D,E,F,G,H,I,J,K,L,M,N,O,P)))

  input_data=readRDS(file=input_data_file)
  input_data$vacc_data[is.na(input_data$vacc_data)]=0.0

  obs_sero_data <- read.csv(file=obs_sero_data_file,header=TRUE)
  obs_sero_data$vc_factor[is.na(obs_sero_data$vc_factor)]=0.7
  obs_case_data <- read.csv(file=obs_case_data_file,header=TRUE)

  enviro_data=read.csv(file=enviro_data_file,header=TRUE)

  input_data=YEP::input_data_process(input_data,obs_sero_data,obs_case_data)

  if(mode_parallel){cluster <- parallel::makeCluster(cores)} else {cluster=NULL}

  set.seed(seed) #For reproducibility
  t1=Sys.time()
  output <- YEP::MCMC(log_params_ini,input_data,obs_sero_data,obs_case_data,filename_prefix,
                      Niter,mode_start,prior_settings,dt,n_reps,enviro_data,
                      p_severe_inf,p_death_severe_inf,add_values,deterministic,
                      mode_parallel,cluster)
  t2=Sys.time()

  if(mode_parallel){parallel::stopCluster(cluster)}

  return(list(duration=t2-t1,seed=seed,filename_prefix=filename_prefix,
              log_params_ini=log_params_ini,output=output))
}
