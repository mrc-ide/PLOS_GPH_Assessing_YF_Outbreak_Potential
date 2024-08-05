comp="C:/Users/kjfras16"
#comp="C:/Users/Work_KJF82"
library("YEP")
library("YEPaux")

dir1a="Documents/00 - Big data files to back up infrequently/00 - YellowFeverDynamics key datasets"
dir1b="Documents/0 - Yellow fever model MCMC results/Model runs 2023-10"
dir2="Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2"

#Load data on individual regions
{
  input_data_regions=readRDS(file=paste(comp,dir1a,"input_data_734_regions_burden.Rds",sep="/"))
  regions=input_data_regions$region_labels
  n_regions=length(regions)
  
  enviro_data_all=read.csv(file=paste(comp,dir1a,"enviro_data_all2.csv",sep="/"),header=TRUE)
  enviro_glm=read.csv(file=paste(comp,dir1a,"enviro_glm_6covs.csv",sep="/"),header=TRUE)
  enviro_data=enviro_data_all[,colnames(enviro_data_all) %in% c("region",colnames(enviro_glm)[enviro_glm==TRUE])]
  enviro_data=enviro_data[enviro_data$region %in% regions,]
  enviro_data=enviro_data[order(enviro_data$region),]
  enviro_data$MIR.max=as.numeric(enviro_data$MIR.max)
}

#Get selected parameter values from chain
{
  chain_data=readRDS(file=paste(comp,dir1b,dir2,"chain_data_combined.Rds",sep="/"))
  n_values=1000
  n_param_sets_all=nrow(chain_data)
  interval=floor((n_param_sets_all-1)/(n_values-1))
  rows=1+(interval*c(0:(n_values-1)))
  assertthat::assert_that(max(rows)<=n_param_sets_all)
  FOI_R0_data_regions=get_mcmc_FOI_R0_data(chain_data[rows,c(2:ncol(chain_data))],type="FOI+R0 enviro",enviro_data)
  FOI_R0_data_regions$country=substr(FOI_R0_data_regions$region,1,3)
  countries_regions=unique(FOI_R0_data_regions$country)
  vaccine_efficacy=chain_data$vaccine_efficacy[rows]
}

#Load country data and calculate FOI/R0 by country
{
  input_data_countries=readRDS(file=paste(comp,dir1a,"input_data_36countries_preventive-default_update_catchup.Rds",sep="/"))
  countries=input_data_countries$region_labels
  assertthat::assert_that(all(countries %in% countries_regions))
  n_countries=length(countries)
  
  FOI_R0_data_country=list(region=countries,FOI=array(NA,dim=c(n_countries,n_values)),R0=array(NA,dim=c(n_countries,n_values)))
  n_years=length(input_data_regions$years_labels)
  for(n_c in c(1:n_countries)){
    FOI_R0_data_subset_country=subset(FOI_R0_data_regions,FOI_R0_data_regions$country==countries[n_c])
    regions_country=unique(FOI_R0_data_subset_country$region)
    n_regions_country=length(regions_country)
    pop_total_by_region=rowSums(input_data_regions$pop_data[regions %in% regions_country,n_years,],2)
    pop_country=sum(pop_total_by_region)
    pop_fraction_by_region=pop_total_by_region/pop_country
    for(i in 1:n_values){
      lines=c(1:n_regions_country)+((i-1)*n_regions_country)
      FOI_R0_data_country$FOI[n_c,i]=sum(FOI_R0_data_subset_country$FOI[lines]*pop_fraction_by_region)
      FOI_R0_data_country$R0[n_c,i]=sum(FOI_R0_data_subset_country$R0[lines]*pop_fraction_by_region)
    }
  }
}

#Set up template
N_age=101
template=data.frame(region=sort(rep(countries,N_age)),year=rep(2018,n_countries*N_age),age_min=rep(c(1:N_age)-1,n_countries),
                    age_max=rep(c(1:N_age),n_countries),life_exp=rep(0,n_countries*N_age))

cluster <- parallel::makeCluster(4)
burden_data <- Generate_Multiple_Datasets(input_data_countries,FOI_values=FOI_R0_data_country$FOI,R0_values=FOI_R0_data_country$R0, 
                                          template, vaccine_efficacy, p_severe_inf = 0.12,p_death_severe_inf = 0.39, 
                                          p_rep_severe = rep(1,n_values),p_rep_death = rep(1,n_values),mode_start = 1,
                                          start_SEIRV = NULL,dt = 1,n_reps = 1,deterministic = TRUE,mode_parallel = "clusterMap",
                                          cluster)
parallel::stopCluster(cluster)

saveRDS(burden_data,file=paste(comp,dir1b,dir2,"data_match/burden_1000sets_1reps.Rds",sep="/"))
#burden_data=readRDS(file=paste(comp,dir1b,dir2,"data_match/burden_1000sets_1reps.Rds",sep="/"))

burden_by_set=rep(NA,n_values)
for(i in 1:n_values){
  burden_data_subset=subset(burden_data,set==i)
  burden_by_set[i]=sum(burden_data_subset$deaths)
}

matplot(sort(burden_by_set),type="l",col=1,xlab="",ylab="Deaths")
matplot(x=c(ceiling(n_values*0.025),ceiling(n_values*0.025)),y=c(min(burden_by_set),max(burden_by_set)),type="l",col=2,add=TRUE)
matplot(x=c(floor(n_values*0.975),floor(n_values*0.975)),y=c(min(burden_by_set),max(burden_by_set)),type="l",col=2,add=TRUE)

#Get median and CI
sets_burden_order=order(burden_by_set)
median_set=sets_burden_order[floor(n_values*0.5)]
burden_median=burden_by_set[median_set]
burden_CI_low=burden_by_set[sets_burden_order[ceiling(n_values*0.025)]]
burden_CI_high=burden_by_set[sets_burden_order[floor(n_values*0.975)]]

#Re-order FOI/R0 values according to burden to get adjusted median
FOI_R0_data_country_median=list(region=countries,FOI=FOI_R0_data_country$FOI[,median_set],R0=FOI_R0_data_country$R0[,median_set])
lines=((median_set-1)*n_regions)+c(1:n_regions)
FOI_R0_data_regions_median=FOI_R0_data_regions[lines,c(2,3,4)]
colnames(FOI_R0_data_regions_median)=c("region","FOI_med_burden","R0_med_burden")
saveRDS(FOI_R0_data_regions_median,file=paste(comp,dir1b,dir2,"FOI_R0_summary_734regions_alt.Rds",sep="/"))
m_FOI_Brazil=chain_data$m_FOI_Brazil[rows[median_set]]
regions_to_adj=c(1:n_regions)[substr(FOI_R0_data_regions_median$region,1,3)=="BRA"]
FOI_R0_data_regions_median$FOI_med_burden[regions_to_adj]=FOI_R0_data_regions_median$FOI_med_burden[regions_to_adj]/m_FOI_Brazil
saveRDS(FOI_R0_data_regions_median,file=paste(comp,dir1b,dir2,"FOI_R0_summary_734regions_alt_no_Brazil_multiplier.Rds",sep="/"))

#Adjust burden results using distribution of p_severe_inf and p_death_severe_inf values from Johansson
# severe disease 0.12 (95% CI 0.05–0.26). The probability of death
# for people experiencing severe disease was 0.47 (95% CI 0.31–0.62)
library(prevalence)
beta_results1 <- betaExpert(0.12, 0.05, 0.26, p = 0.95, method = "mean")
beta_results2 <- betaExpert(0.47, 0.31, 0.62, p = 0.95, method = "mean")
set.seed(1)
p_severe_inf_values=rbeta(1000,beta_results1$alpha, beta_results1$beta)
p_death_severe_inf_values=rbeta(1000,beta_results2$alpha, beta_results2$beta)
infs_old=burden_by_set/(0.12*0.39)
burden_new=infs_old*p_severe_inf_values*p_death_severe_inf_values

matplot(sort(burden_new),type="l",col=1,xlab="",ylab="Deaths")
matplot(x=c(ceiling(n_values*0.025),ceiling(n_values*0.025)),y=c(min(burden_new),max(burden_new)),type="l",col=2,add=TRUE)
matplot(x=c(floor(n_values*0.975),floor(n_values*0.975)),y=c(min(burden_new),max(burden_new)),type="l",col=2,add=TRUE)

#Get median and CI
alt_sets_burden_order=order(burden_new)
alt_median_set=alt_sets_burden_order[floor(n_values*0.5)]
alt_burden_median=burden_new[alt_median_set]
alt_burden_CI_low=burden_new[alt_sets_burden_order[ceiling(n_values*0.025)]]
alt_burden_CI_high=burden_new[alt_sets_burden_order[floor(n_values*0.975)]]
