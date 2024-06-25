comp="C:/Users/kjfras16"
library(YEPaux)
library(Rmisc)
library(ggplot2)
#Data
{
  dir1="Documents/00 - Big data files to back up infrequently/00 - YellowFeverDynamics key datasets"
  obs_sero_data=read.csv(file=paste(comp,dir1,"sero_data_af2.csv",sep="/"),header=TRUE)
  obs_case_data=read.csv(file=paste(comp,dir1,"case_data_la_omit_COL26_3_6countries.csv",sep="/"),header=TRUE)
  obs_outbreak_data=NULL

  input_data=readRDS(file=paste(comp,dir1,"input_data_272regions_case_sero.Rds",sep="/"))
  regions=input_data$region_labels
  
  dir2=paste0("Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/data/",
              "Real - Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2")
  model_data=readRDS(file=paste(comp,dir2,"data_match1000.Rds",sep="/"))
}

#Adjust reported cases/deaths based on distribution of p_severe_inf and p_death_severe_inf values
{
  chain_data_all=readRDS(file=paste(comp,dir2,"chain_data_combined.Rds",sep="/"))
  n_values=length(model_data)
  n_param_sets_all=nrow(chain_data_all)
  interval=floor((n_param_sets_all-1)/(n_values-1))
  rows=1+(interval*c(0:(n_values-1)))
  assertthat::assert_that(max(rows)<=n_param_sets_all)
  p_rep_severe_values=chain_data_all$p_rep_severe[rows]
  p_rep_death_values=chain_data_all$p_rep_death[rows]
  
  cfr_data=readRDS(file=paste0(comp,"/Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/data/",
                               "P_severe_severeDeath_1000.RDS"))
  p_inf_rep_case_old=0.12*(((1.0-0.39)*p_rep_severe_values)+(0.39*p_rep_death_values))
  p_inf_rep_case_new=cfr_data$P_severe*(((1.0-cfr_data$P_severeDeath)*p_rep_severe_values)+
                                      (cfr_data$P_severeDeath*p_rep_death_values))
  p_inf_rep_death_new=cfr_data$P_severe*cfr_data$P_severeDeath*p_rep_death_values
  
  model_data_new=model_data
  for(i in 1:n_values){
    model_infs=model_data[[i]]$model_case_values/p_inf_rep_case_old[i]
    #model_infs_alt=model_data[[i]]$model_death_values/(0.12*0.39*p_rep_death_values[i])
    model_data_new[[i]]$model_case_values=model_infs*p_inf_rep_case_new[i]
    model_data_new[[i]]$model_death_values=model_infs*p_inf_rep_death_new[i]
  }
}

dir3="Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/images/Real - main text"

case_graphs <- case_match_graphs(model_data_new,obs_case_data,input_data,plot_type="all",text_size=20)

years_selected=c("1990","","","","","1995","","","","","2000","","","","","2005","","","","","2010","","","","","2015")
country_names=c("Bolivia","Brazil","Colombia","Ecuador","Peru","Venezuela")
for(i in 1:length(case_graphs[[1]])){
  case_graphs$cases_graphs[[i]] <- case_graphs$cases_graphs[[i]]+scale_x_continuous(name="",
                breaks=c(min(case_graphs$cases_graphs[[i]]$data$years):max(case_graphs$cases_graphs[[i]]$data$years)),
                labels=years_selected) + labs(title=country_names[i])
  case_graphs$deaths_graphs[[i]] <- case_graphs$deaths_graphs[[i]]+scale_x_continuous(name="",
                breaks=c(min(case_graphs$deaths_graphs[[i]]$data$years):max(case_graphs$deaths_graphs[[i]]$data$years)),
                labels=years_selected) + labs(title=country_names[i])
}

png(filename=paste(comp,dir3,"real_match_annual_cases_adj.png",sep="/"),width=1440,height=840)
multiplot(plotlist=case_graphs$cases_graphs,cols=floor(sqrt(length(case_graphs$cases_graphs))))
dev.off()
png(filename=paste(comp,dir3,"real_match_annual_deaths_adj.png",sep="/"),width=1440,height=840)
multiplot(plotlist=case_graphs$deaths_graphs,cols=floor(sqrt(length(case_graphs$deaths_graphs))))
dev.off()
