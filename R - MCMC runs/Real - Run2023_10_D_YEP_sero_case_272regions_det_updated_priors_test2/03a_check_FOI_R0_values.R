setwd("T:/Keith/Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2")
#setwd("T:/Keith/Run2023_10_A_YEP_sero_case_272regions_6cov_det_newpriors")
library(YEPaux)

{
  data_folder="C:/Users/kjfras16/Documents/00 - Big data files to back up infrequently/00 - YellowFeverDynamics key datasets"
  
  input_data=readRDS(file=paste(data_folder,"input_data_734_regions_burden.Rds",sep="/"))
  regions=input_data$region_labels
  n_regions=length(regions)
  
  enviro_data_all=read.csv(file=paste(data_folder,"enviro_data_all2.csv",sep="/"),header=TRUE)
  enviro_glm=read.csv(file=paste(data_folder,"enviro_glm_6covs.csv",sep="/"),header=TRUE)
  enviro_data=enviro_data_all[,colnames(enviro_data_all) %in% c("region",colnames(enviro_glm)[enviro_glm==TRUE])]
  enviro_data=enviro_data[enviro_data$region %in% regions,]
  enviro_data=enviro_data[order(enviro_data$region),]
  enviro_data$MIR.max=as.numeric(enviro_data$MIR.max)
}

{
  shapefile_folder="C:/Users/kjfras16/Documents/00 - Big data files to back up infrequently/00 - GADM36 shapefiles"
  country_list_af=c("AGO","BDI","BEN","BFA","CAF","CIV","CMR","COD","COG","ERI","ETH","GAB","GHA","GIN","GMB","GNB","GNQ",
                    "KEN","LBR","MLI","MRT","NER","NGA","RWA","SDN","SEN","SLE","SOM","SSD","TCD","TGO","TZA","UGA","ZMB")
  country_list_sa=c("ARG","BOL","BRA","COL","ECU","GUF","GUY","PER","PRY","SUR","VEN")
  regions_af=regions[substr(regions,1,3) %in% country_list_af]
  regions_sa=regions[substr(regions,1,3) %in% country_list_sa]
  shapefiles_af=rep("",length(country_list_af))
  for(i in 1:length(country_list_af)){
    shapefiles_af[i]=paste(shapefile_folder,"/",country_list_af[i],"/gadm36_",country_list_af[i],"_1.shp",sep="")
  }
  shape_data_af=map_shapes_load(regions_af, shapefiles_af, region_label_type="GID_1")
  shapefiles_sa=rep("",length(country_list_sa))
  for(i in 1:length(country_list_sa)){
    shapefiles_sa[i]=paste(shapefile_folder,"/",country_list_sa[i],"/gadm36_",country_list_sa[i],"_1.shp",sep="")
  }
  shape_data_sa=map_shapes_load(regions_sa, shapefiles_sa, region_label_type="GID_1")  
}

prelim_fit_results=readRDS(file="prelim_fit/prelim_fit_results02.Rds")
iterations_select=c(1:10)
for(i in iterations_select){
  if(i==iterations_select[1]){results_it_select=prelim_fit_results[[i]]}else{
    results_it_select=rbind(results_it_select,prelim_fit_results[[i]])
  }
}
results_it_select=results_it_select[order(-results_it_select$posterior),] #Sort all by posterior

n_values=4 #Number of values to select from top of posterior-ordered list
n_params=16
params=results_it_select[c(1:n_values),1+c(1:n_params)]
FOI_R0_values=get_mcmc_FOI_R0_data(params,type="FOI+R0 enviro",enviro_data)
max(FOI_R0_values$FOI)*365
max(FOI_R0_values$R0)

FOI_R0_dist_data=get_FOI_R0_dist_data(FOI_R0_values)
FOI_R0_median_data=data.frame(region=FOI_R0_dist_data$region,FOI=FOI_R0_dist_data$FOI_50*365,R0=FOI_R0_dist_data$R0_50)
epi_data_af=subset(FOI_R0_median_data,region %in% regions_af)
epi_data_sa=subset(FOI_R0_median_data,region %in% regions_sa)
FOI_values_af=epi_data_af$FOI
R0_values_af=epi_data_af$R0
FOI_values_sa=epi_data_sa$FOI
R0_values_sa=epi_data_sa$R0

colour_scheme=readRDS(file=paste(path.package("YEPaux"), "exdata/colour_scheme_example.Rds", sep="/"))
map_colour_scale=colour_scheme$colour_scale
scale_FOI=exp(pretty(log(FOI_R0_median_data$FOI),10))
scale_R0=pretty(FOI_R0_median_data$R0,10)

create_map(shape_data_af,FOI_values_af,scale=scale_FOI,colour_scale=map_colour_scale,pixels_max=1440,
           text_size=1.5,map_title="",legend_title="Spillover force of infection (annual)",legend_position="bottomleft",
           legend_format="e",legend_dp=1,output_file="FOI_map_af_med.png")
create_map(shape_data_sa,FOI_values_sa,scale=scale_FOI,colour_scale=map_colour_scale,pixels_max=1440,
           text_size=1.5,map_title="",legend_title="Spillover force of infection (annual)",legend_position="bottomright",
           legend_format="e",legend_dp=1,output_file="FOI_map_sa_med.png")

create_map(shape_data_af,R0_values_af,scale=scale_R0,colour_scale=map_colour_scale,pixels_max=1440,
           text_size=1.5,map_title="",legend_title="Basic reproduction number",legend_position="bottomleft",
           legend_format="f",legend_dp=2,output_file="R0_map_af_med.png")
create_map(shape_data_sa,R0_values_sa,scale=scale_R0,colour_scale=map_colour_scale,pixels_max=1440,
           text_size=1.5,map_title="",legend_title="Basic reproduction number",legend_position="bottomright",
           legend_format="f",legend_dp=2,output_file="R0_map_sa_med.png")

param_sets=data.frame(n=c(1:n_values),filename_prefix=rep("",n_values))
for(i in 1:n_values){param_sets$filename_prefix[i]=paste("outputs/",LETTERS[i],"/Chain",LETTERS[i],sep="")}

param_sets=cbind(param_sets,params)
colnames(param_sets)=NULL
write.csv(param_sets,file="exdata/chain_inputs_prelim_fit02_best4.csv",row.names=FALSE)