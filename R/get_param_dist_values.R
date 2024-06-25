library(YEPaux)
comp="C:/Users/kjfras16"
data_folder=paste0("Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/data",
                   "/Real - Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2")

chain_data=readRDS(file=paste(comp,data_folder,"chain_data_combined.Rds",sep="/"))

n_params=ncol(chain_data)-1
n_pts=nrow(chain_data)
n_025=ceiling(n_pts*0.025)
n_975=floor(n_pts*0.975)
param_names=colnames(chain_data)[1+c(1:n_params)]
output_frame=data.frame(param=param_names,value_median=rep(NA,n_params),value_025=rep(NA,n_params),value_975=rep(NA,n_params))

for(i in 1:n_params){
  values=sort(chain_data[,i+1])
  output_frame$value_median[i]=median(values)
  output_frame$value_025[i]=values[n_025]
  output_frame$value_975[i]=values[n_975]
}

write.csv(output_frame,file="data/Real - Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2/param_dist.csv",
          row.names=FALSE)