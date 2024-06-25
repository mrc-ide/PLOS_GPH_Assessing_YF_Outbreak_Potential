library(YEPaux)
comp="C:/Users/kjfras16"
data_folder=paste0("Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/data",
                   "/Real - Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2")
data_folder2="Documents/00 - Big data files to back up infrequently/00 - YellowFeverDynamics key datasets"
shapefile_folder="Documents/00 - Big data files to back up infrequently/00 - GADM36 shapefiles"
results_folder=paste0("Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/",
                      "images/Real - main text")

chain_data=readRDS(file=paste(comp,data_folder,"chain_data_combined.Rds",sep="/"))
input_data_all=readRDS(file=paste(comp,data_folder2,"input_data_734_regions_burden.Rds",sep="/"))
enviro_data_all=read.csv(file=paste(comp,data_folder2,"enviro_data_all2.csv",sep="/"),header=TRUE)
enviro_glm=read.csv(file=paste(comp,data_folder2,"enviro_glm_6covs.csv",sep="/"),header=TRUE)
{
  #Subset to just African regions
  country_list_af=c("AGO","BDI","BEN","BFA","CAF","CIV","CMR","COD","COG","ERI","ETH","GAB","GHA","GIN","GMB","GNB","GNQ",
                    "KEN","LBR","MLI","MRT","NER","NGA","RWA","SDN","SEN","SLE","SOM","SSD","TCD","TGO","TZA","UGA","ZMB")
  regions_af=input_data_all$region_labels[substr(input_data_all$region_labels,1,3) %in% country_list_af]
  n_regions=length(regions_af)
  input_data=YEP::input_data_truncate(input_data_all,regions_new=regions_af)
  
  enviro_data=enviro_data_all[,colnames(enviro_data_all) %in% c("region",colnames(enviro_glm)[enviro_glm==TRUE])]
  enviro_data=enviro_data[enviro_data$region %in% input_data$region_labels,]
  enviro_data=enviro_data[order(enviro_data$region),]
  enviro_data$MIR.max=as.numeric(enviro_data$MIR.max)
  FOI_R0_values=get_mcmc_FOI_R0_data(chain_data[,c(2:ncol(chain_data))],type="FOI+R0 enviro",enviro_data)
  n_lines_total=nrow(FOI_R0_values)
  n_pts=n_lines_total/n_regions
  
  pop_data=input_data$pop_data[,length(input_data$years_labels),]
  pop_region_totals=pop_ratio=rep(NA,n_regions)
  for(n_region in 1:n_regions){
    pop_region_totals[n_region]=sum(pop_data[n_region,])
    pop_ratio[n_region]=sum(pop_data[n_region,c(2:61)])/pop_region_totals[n_region]
  }
  pop_sum_all=sum(pop_data)
}

#Load shapefiles
{
  shapefiles_af=rep("",length(country_list_af))
  for(i in 1:length(country_list_af)){
    shapefiles_af[i]=paste(comp,"/",shapefile_folder,"/",country_list_af[i],"/gadm36_",country_list_af[i],"_1.shp",sep="")
  }
  shape_data_af=map_shapes_load(regions_af, shapefiles_af, region_label_type="GID_1")
}

vacc_eff_values=0.01*c(50:100)
coverage_values=0.01*c(50:80)
protection_data=data.frame(array(NA,dim=c(length(vacc_eff_values)*length(coverage_values),8)))
colnames(protection_data)=c("vacc_eff","coverage","type1_05","type1_07","type1_10","type2_05","type2_07","type2_10")
protection_data$vacc_eff=sort(rep(vacc_eff_values,length(coverage_values)))
protection_data$coverage=rep(coverage_values,length(vacc_eff_values))
probs=flags_threshold=data.frame(array(NA,dim=c(n_regions,4)))
colnames(probs)=c("regions","P(Reff>0.5)","P(Reff>0.7)","P(Reff>1.0)")
colnames(flags_threshold)=c("regions","P(Reff>0.5)>0.05","P(Reff>0.7)>0.05","P(Reff>1.0)>0.05")

i=0
for(vacc_eff in vacc_eff_values){
  for(coverage in coverage_values){
    i=i+1
    cat("\nvacc_eff:\t",vacc_eff,"\tcoverage:\t",coverage,sep="")
    protected_fraction_regions=coverage*vacc_eff*pop_ratio
    lines0=n_regions*c(0:(n_pts-1))
    for(n_region in 1:n_regions){
      R_eff_values=FOI_R0_values$R0[lines0+n_region]*(1-protected_fraction_regions[n_region])
      probs$`P(Reff>0.5)`[n_region]=sum(R_eff_values>0.5)
      probs$`P(Reff>0.7)`[n_region]=sum(R_eff_values>0.7)
      probs$`P(Reff>1.0)`[n_region]=sum(R_eff_values>1.0)
    }
    probs$`P(Reff>0.5)`=probs$`P(Reff>0.5)`/n_pts
    probs$`P(Reff>0.7)`=probs$`P(Reff>0.7)`/n_pts
    probs$`P(Reff>1.0)`=probs$`P(Reff>1.0)`/n_pts
    flags_threshold$`P(Reff>0.5)>0.05`=as.numeric(probs$`P(Reff>0.5)`>0.05)
    flags_threshold$`P(Reff>0.7)>0.05`=as.numeric(probs$`P(Reff>0.7)`>0.05)
    flags_threshold$`P(Reff>1.0)>0.05`=as.numeric(probs$`P(Reff>1.0)`>0.05)
    protection_data$type1_05[i]=1.0-(sum(flags_threshold$`P(Reff>0.5)>0.05`*pop_region_totals)/pop_sum_all)
    protection_data$type1_07[i]=1.0-(sum(flags_threshold$`P(Reff>0.7)>0.05`*pop_region_totals)/pop_sum_all)
    protection_data$type1_10[i]=1.0-(sum(flags_threshold$`P(Reff>1.0)>0.05`*pop_region_totals)/pop_sum_all)
    protection_data$type2_05[i]=1.0-(sum(probs$`P(Reff>0.5)`*pop_region_totals)/pop_sum_all)
    protection_data$type2_07[i]=1.0-(sum(probs$`P(Reff>0.7)`*pop_region_totals)/pop_sum_all)
    protection_data$type2_10[i]=1.0-(sum(probs$`P(Reff>1.0)`*pop_region_totals)/pop_sum_all)
  }
}
protection_data[,c(3:8)]=round(protection_data[,c(3:8)]*100,2)
write.csv(protection_data,file="R/protection_data.csv",row.names=FALSE)
suffixes=c("R<0.5 (>5% probability Y/N)","R<0.7 (>5% probability Y/N)","R<1.0 (>5% probability Y/N)",
           "R<0.5 (probability)","R<0.7 (probability)","R<1.0 (probability)")

par(mar=c(4,4,4,4))
for(i in 3:8){
  protection_matrix=matrix(protection_data[,i],nrow=length(coverage_values),ncol=length(vacc_eff_values))
  filled.contour(x=vacc_eff_values,y=coverage_values,z=t(protection_matrix),color.palette = terrain.colors,zlim=c(0,100))#,
                 #xlab="Effectiveness of reported vaccinations",ylab="Reported vaccination coverage")
  title(main=paste0("Fraction of population protected based on criterion:\n",suffixes[i-2]),
        xlab="Effectiveness of reported vaccinations",ylab="Reported vaccination coverage")
}