#OLD - Use file ending in_alt.R

library(YEPaux)
comp="C:/Users/kjfras16"
data_folder=paste0("Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/data",
                   "/Real - Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2")
data_folder2="Documents/00 - Big data files to back up infrequently/00 - YellowFeverDynamics key datasets"
shapefile_folder="Documents/00 - Big data files to back up infrequently/00 - GADM36 shapefiles"
results_folder=paste0("Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/",
                      "images/Real - main text/Reff prob maps calculated vacceff")

chain_data=readRDS(file=paste(comp,data_folder,"chain_data_combined.Rds",sep="/"))
input_data_all=readRDS(file=paste(comp,data_folder2,"input_data_734_regions_burden.Rds",sep="/"))
enviro_data_all=read.csv(file=paste(comp,data_folder2,"enviro_data_all2.csv",sep="/"),header=TRUE)
enviro_glm=read.csv(file=paste(comp,data_folder2,"enviro_glm_6covs.csv",sep="/"),header=TRUE)
{
  regions=input_data_all$region_labels
  n_regions=length(regions)
  enviro_data=enviro_data_all[,colnames(enviro_data_all) %in% c("region",colnames(enviro_glm)[enviro_glm==TRUE])]
  enviro_data=enviro_data[enviro_data$region %in% input_data_all$region_labels,]
  enviro_data=enviro_data[order(enviro_data$region),]
  enviro_data$MIR.max=as.numeric(enviro_data$MIR.max)
  FOI_R0_values=get_mcmc_FOI_R0_data(chain_data[,c(2:ncol(chain_data))],type="FOI+R0 enviro",enviro_data)
  n_lines_total=nrow(FOI_R0_values)
  n_pts=n_lines_total/n_regions
  
  pop_data=input_data_all$pop_data[,length(input_data_all$years_labels),]
  pop_ratio=rep(NA,n_regions)
  for(n_region in 1:n_regions){
    pop_ratio[n_region]=sum(pop_data[n_region,c(2:61)])/sum(pop_data[n_region,])
  }
}

{
  country_list_af=c("AGO","BDI","BEN","BFA","CAF","CIV","CMR","COD","COG","ERI","ETH","GAB","GHA","GIN","GMB","GNB","GNQ",
                    "KEN","LBR","MLI","MRT","NER","NGA","RWA","SDN","SEN","SLE","SOM","SSD","TCD","TGO","TZA","UGA","ZMB")
  country_list=names(table(substr(regions,1,3)))
  assertthat::assert_that(all(country_list_af %in% country_list))
  regions_af=regions[substr(regions,1,3) %in% country_list_af]
  
  shapefiles_af=shapefiles_af_countries=rep("",length(country_list_af))
  for(i in 1:length(country_list_af)){
    shapefiles_af[i]=paste(comp,"/",shapefile_folder,"/",country_list_af[i],"/gadm36_",country_list_af[i],"_1.shp",sep="")
    shapefiles_af_countries[i]=paste(comp,"/",shapefile_folder,"/",country_list_af[i],"/gadm36_",country_list_af[i],"_0.shp",
                                     sep="")
  }
  shape_data_af=map_shapes_load(regions_af, shapefiles_af, region_label_type="GID_1")
  shape_data_af_countries=map_shapes_load(country_list_af, shapefiles_af_countries, region_label_type="GID_0")
}

for(coverage_threshold in c(0.5,0.6,0.8)){
  #Get threshold R0 values for R>=1, R>= 0.7 and R>=0.5 based on vaccine coverage in 1-60 year olds
  {
    R0_threshold1=R0_threshold2=R0_threshold3=rep(NA,n_regions)
    for(n_region in 1:n_regions){
      R0_threshold1[n_region]=0.5/(1.0-(coverage_threshold*pop_ratio[n_region]))
      R0_threshold2[n_region]=0.7/(1.0-(coverage_threshold*pop_ratio[n_region]))
      R0_threshold3[n_region]=1.0/(1.0-(coverage_threshold*pop_ratio[n_region]))
    }
    prob_data=data.frame(region=regions)
    prob_data$`P(Reff>1.0)`=prob_data$`P(Reff>0.7)`=prob_data$`P(Reff>0.5)`=rep(NA,n_regions)
    lines0=n_regions*c(0:(n_pts-1))
    for(n_region in 1:n_regions){
      R0_values=FOI_R0_values$R0[lines0+n_region]
      prob_data$`P(Reff>0.5)`[n_region]=length(R0_values[R0_values>=R0_threshold1[n_region]])
      prob_data$`P(Reff>0.7)`[n_region]=length(R0_values[R0_values>=R0_threshold2[n_region]])
      prob_data$`P(Reff>1.0)`[n_region]=length(R0_values[R0_values>=R0_threshold3[n_region]])
    }
    prob_data$`P(Reff>0.5)`=prob_data$`P(Reff>0.5)`/n_pts
    prob_data$`P(Reff>0.7)`=prob_data$`P(Reff>0.7)`/n_pts
    prob_data$`P(Reff>1.0)`=prob_data$`P(Reff>1.0)`/n_pts
    prob_data_af=subset(prob_data,region %in% regions_af)
    #prob_data_sa=subset(prob_data,region %in% regions_sa)
  }
  
  colour_scheme=readRDS(file=paste(path.package("YEPaux"), "exdata/colour_scheme_example.Rds", sep="/"))
  colour_scale=colour_scheme$colour_scale
  scale=c(0,1e-5,1e-4,1e-3,1e-2,0.1,0.25,0.5,0.75,0.9,0.99,1.001)
  
  data_types=colnames(prob_data)[c(2:ncol(prob_data))]
  
  for(data_select in 1:length(data_types)){
    data_type=data_types[data_select]
    values_af=prob_data_af[,1+data_select]
    #values_sa=prob_data_sa[,1+data_select]
    
    pc=round(coverage_threshold*100,0)
    create_map(shape_data_af,values_af,scale=scale,colour_scale,pixels_max=1440,text_size=2,
               border_colour_regions="light grey",additional_border_shapes=shape_data_af_countries,
               border_colour_additional="black",legend_title=data_types[data_select],legend_position="bottomleft",
               legend_format="e",legend_dp=1,
               output_file=paste0(comp,"/",results_folder,"/Reff_",pc,"_probs_map_af",data_select,".png",sep=""))
    # create_map(shape_data_sa,values_sa,scale=scale,colour_scale,pixels_max=1440,text_size=2,map_title="",
    #            legend_title=data_types[data_select],legend_position="bottomright",legend_format="e",legend_dp=1,
    #            output_file=paste0(comp,"/",results_folder,"/Reff_",pc,"_probs_map_sa",data_select,".png",sep=""))
  }
}