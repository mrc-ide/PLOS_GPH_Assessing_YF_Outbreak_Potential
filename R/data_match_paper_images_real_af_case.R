comp="C:/Users/kjfras16"
devtools::load_all(path=paste0(comp,"/Documents/GitHub/YEPaux"))
library(Rmisc)
library(ggplot2)
#Data
{
  dir1="Documents/00 - Big data files to back up infrequently/00 - YellowFeverDynamics key datasets"
  obs_case_data=read.csv(file=paste(comp,dir1,"case_data_af_adm1_2000_2020.csv",sep="/"),header=TRUE)
  obs_case_data$deaths=rep(0,nrow(obs_case_data))
  regions_combined=unique(obs_case_data$region)
  regions=c()
  for(i in 1:length(regions_combined)){regions=append(regions,strsplit(regions_combined[i],",")[[1]])}
  
  dir2=paste0("Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/data/",
              "Real - Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2")
  model_data=readRDS(file=paste(comp,dir2,"data_match1000_vs_af_data.Rds",sep="/"))
  
  input_data=readRDS(file=paste(comp,dir1,"input_data_224_regions_case_sero_af.Rds",sep="/"))
  input_data=YEP::input_data_truncate(input_data,regions)
}

dir3="Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/images/Real - SI"

#NB - Temporarily change colour settings in case_match_graphs
case_graphs <- case_match_graphs(model_data,obs_case_data,input_data,plot_type="all",text_size=20)

years_selected=c("2000","","","","","2005","","","","","2010","","","","","2015","","","","","2020")
#country_names=c("BFA","CAF","CIV","CMR","COD","GHA","GIN","LBR","MLI","SEN","SLE")
country_names=c("Burkina Faso","Central African Republic","Ivory Coast","Cameroon","Democratic Republic of the Congo",
                "Ghana","Guinea","Liberia","Mali","Senegal","Sierra Leone")
for(i in 1:length(case_graphs[[1]])){
  case_graphs$cases_graphs[[i]] <- case_graphs$cases_graphs[[i]]+scale_x_continuous(name="",
                breaks=c(min(case_graphs$cases_graphs[[i]]$data$years):max(case_graphs$cases_graphs[[i]]$data$years)),
                labels=years_selected) + labs(title=country_names[i])
}

png(filename=paste(comp,dir3,"validation_case_data_af.png",sep="/"),width=1440,height=1920)
multiplot(plotlist=case_graphs$cases_graphs,cols=floor(sqrt(length(case_graphs$cases_graphs))))
dev.off()