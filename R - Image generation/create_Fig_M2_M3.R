comp="C:/Users/kjfras16"
#library("YEPaux")
devtools::load_all(paste0(comp,"/Documents/GitHub/YEPaux"))
library(Rmisc)
library(ggplot2)
#Data
{
  dir1="Documents/00 - Big data files to back up infrequently/00 - YellowFeverDynamics key datasets"
  obs_sero_data=read.csv(file=paste(comp,dir1,"sero_data_af3.csv",sep="/"),header=TRUE)
  # for(i in 1:nrow(obs_sero_data)){
  #   obs_sero_data$country_zone[i]=paste(obs_sero_data$country[i],obs_sero_data$year[i],sep=" ")}
  obs_case_data=read.csv(file=paste(comp,dir1,"case_data_la_omit_COL26_3_6countries.csv",sep="/"),header=TRUE)
  obs_outbreak_data=NULL

  input_data=readRDS(file=paste(comp,dir1,"input_data_272regions_case_sero.Rds",sep="/"))
  regions=input_data$region_labels
  
  dir2=paste0("Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/data/",
              "Real - Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2")
  model_data=readRDS(file=paste(comp,dir2,"data_match1000.Rds",sep="/"))
}

dir3="Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/images/Real - main text"

sero_graphs <- sero_match_graphs(model_data,obs_sero_data,plot_type="all",text_size=15)

png(filename=paste(comp,dir3,"real_match_sero.png",sep="/"),width=2160,height=2160)
multiplot(plotlist=sero_graphs,cols=4)#floor(sqrt(length(sero_graphs))))
dev.off()

case_graphs <- case_match_graphs(model_data,obs_case_data,input_data,plot_type="all",text_size=20)

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

png(filename=paste(comp,dir3,"real_match_annual_cases.png",sep="/"),width=1440,height=840)
multiplot(plotlist=case_graphs$cases_graphs,cols=floor(sqrt(length(case_graphs$cases_graphs))))
dev.off()
png(filename=paste(comp,dir3,"real_match_annual_deaths.png",sep="/"),width=1440,height=840)
multiplot(plotlist=case_graphs$deaths_graphs,cols=floor(sqrt(length(case_graphs$deaths_graphs))))
dev.off()
