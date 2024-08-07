explore_param_space_priors <- function(n_iterations=1,n_param_sets=1,n_bounds=1,type=NULL,log_params_min=NULL,
                            log_params_max=NULL,input_data=list(),obs_sero_data=list(),obs_case_data=list(),
                            mode_start=0,prior_settings=list(type="zero"),dt=1.0,n_reps=1,enviro_data=NULL,R0_fixed_values=c(),
                            p_severe_inf = 0.12, p_death_severe_inf = 0.39,
                            add_values=list(vaccine_efficacy=1.0,p_rep_severe=1.0,p_rep_death=1.0,m_FOI_Brazil=1.0)){

  #TODO - Add assertthat functions
  assert_that(mode_start %in% c(0,1,3),msg="mode_start must have value 0, 1 or 3")
  assert_that(length(log_params_min)==length(log_params_max),msg="Parameter limit vectors must have same lengths")
  assert_that(type %in% c("FOI+R0","FOI","FOI+R0 enviro","FOI enviro"))
  assert_that(prior_settings$type %in% c("zero","exp","norm"),msg="Prior type must be 'zero', 'exp' or 'norm'")

  explore_results=list()
  n_params=length(log_params_min)

  #Get additional values
  extra_estimated_params=c()
  add_value_names=names(add_values)
  #assert_that(all(add_value_names==c("vaccine_efficacy","p_rep_severe","p_rep_death","m_FOI_Brazil"))) #TBV in alt version
  for(var_name in add_value_names){
    if(is.null(add_values[[var_name]])==TRUE){extra_estimated_params=append(extra_estimated_params,var_name)}
  }
  param_names=create_param_labels(type,input_data,enviro_data,extra_estimated_params)

  assert_that(length(param_names)==n_params)
  names(log_params_min)=names(log_params_max)=param_names
  xlabels=param_names
  for(i in 1:n_params){xlabels[i]=substr(xlabels[i],1,15)}
  ylabels=10^c(-8,-6,-4,-3,-2,-1,0,1)
  par(mar=c(6,2,1,1))
  ylim=c(min(log_params_min),max(log_params_max))

  for(iteration in 1:n_iterations){
    cat("\nIteration: ",iteration,"\n",sep="")
    all_param_sets <- lhs(n=n_param_sets,rect=cbind(log_params_min,log_params_max))
    results=data.frame()
    consts=list(type=type,mode_start=mode_start,prior_settings=prior_settings,
                dt=dt,n_reps=n_reps,enviro_data=enviro_data,R0_fixed_values=R0_fixed_values,
                p_severe_inf = p_severe_inf, p_death_severe_inf = p_death_severe_inf,add_values=add_values)

    for(set in 1:n_param_sets){
      cat("\n\tSet: ",set,sep="")
      log_params_prop=all_param_sets[set,]

      cat("\n\tParams: ",signif(log_params_prop,3))

      names(log_params_prop)=param_names
      prior_value=prior_calc(log_params_prop,input_data,obs_sero_data,obs_case_data,consts)
      results<-rbind(results,c(set,exp(log_params_prop),prior_value))
      if(set==1){colnames(results)=c("set",param_names,"prior")}

      cat("\n\tPrior = ",prior_value,sep="")

    }
    results<-results[order(results$prior,decreasing=TRUE), ]
    explore_results[[iteration]]=results

    log_params_min_new=log_params_max_new=rep(0,n_params)
    for(i in 1:n_params){
      log_params_min_new[i]=min(log(results[c(1:n_bounds),i+1]))
      log_params_max_new[i]=max(log(results[c(1:n_bounds),i+1]))
    }
    names(log_params_min_new)=names(log_params_max_new)=param_names

    matplot(x=c(1:n_params),y=log(t(results[c(1:n_bounds),c(1:n_params)+1])),type="p",pch=16,col=1,
            xaxt="n",yaxt="n",xlab="",ylab="",ylim=ylim)
    axis(side=1,at=c(1:n_params),labels=xlabels,las=2,cex.axis=0.7)
    axis(side=2,at=log(ylabels),labels=ylabels)
    matplot(x=c(1:n_params),y=log_params_min,type="l",col=1,lty=2,add=TRUE)
    matplot(x=c(1:n_params),y=log_params_max,type="l",col=1,lty=2,add=TRUE)
    matplot(x=c(1:n_params),y=log_params_min_new,type="l",col=2,add=TRUE)
    matplot(x=c(1:n_params),y=log_params_max_new,type="l",col=2,add=TRUE)

    log_params_min=log_params_min_new
    log_params_max=log_params_max_new
  }

  return(explore_results)
}
#-------------------------------------------------------------------------------
prior_calc <- function(log_params_prop=c(),input_data=list(),obs_sero_data=NULL,obs_case_data=NULL,
                       consts=list()){

  vaccine_efficacy=p_rep_severe=p_rep_death=p_rep_severe_af=p_rep_death_af=p_rep_severe_sa=p_rep_death_sa=m_FOI_Brazil=1.0
  prior_add=0
  for(var_name in names(consts$add_values)){
    if(is.numeric(consts$add_values[[var_name]])==FALSE){
      i=match(var_name,names(log_params_prop))
      value=exp(as.numeric(log_params_prop[i]))
      assign(var_name,value)
      if(consts$prior_settings$type=="norm"){
        prior_add=prior_add+log(dtrunc(value,"norm",a=0,b=1,mean=consts$prior_settings$norm_params_mean[i],
                                       sd=consts$prior_settings$norm_params_sd[i]))
      } else {
        if(consts$prior_settings$type=="flat"){
          if(value<consts$prior_settings$log_params_min[i] || value>consts$prior_settings$log_params_max[i]){prior_add=-Inf}
        }
      }
    } else {assign(var_name,consts$add_values[[var_name]])}
  }

  #If additional values give finite prior, get FOI and R0 values and calculate associated prior
  if(is.finite(prior_add)){
    regions=input_data$region_labels
    n_regions=length(regions)

    FOI_R0_data=mcmc_FOI_R0_setup(consts$type,consts$prior_settings,regions,log_params_prop,
                                  consts$enviro_data,consts$R0_fixed_values)
    FOI_values=FOI_R0_data$FOI_values

    for(n_region in 1:n_regions){if(substr(regions[n_region],1,3)=="BRA"){FOI_values[n_region]=FOI_values[n_region]*m_FOI_Brazil}}
    R0_values=FOI_R0_data$R0_values
    if(consts$prior_settings$type=="norm"){
      prior_prop=FOI_R0_data$prior + prior_add +
        sum(log(dtrunc(R0_values,"norm",a=0,b=Inf,mean=consts$prior_settings$R0_mean,sd=consts$prior_settings$R0_sd))) +
        sum(log(dtrunc(FOI_values,"norm",a=0,b=1,mean=consts$prior_settings$FOI_mean,sd=consts$prior_settings$FOI_sd)))
    } else {
      prior_prop=FOI_R0_data$prior+prior_add
    }
  } else {prior_prop=-Inf}

  return(prior_prop)
}
