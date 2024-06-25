library(YEPaux)
comp="C:/Users/kjfras16"
data_folder=paste0("Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/data",
                   "/Real - Run2023_10_D_YEP_sero_case_272regions_det_updated_priors_test2")
shapefile_folder="Documents/00 - Big data files to back up infrequently/00 - GADM36 shapefiles"
results_folder=paste0("Documents/0 - Publications+Reports/Paper 07 - Yellow fever model paper revision/",
                      "images/Real - main text")

country_list_af=c("AGO","BDI","BEN","BFA","CAF","CIV","CMR","COD","COG","ERI","ETH","GAB","GHA","GIN","GMB","GNB","GNQ",
                  "KEN","LBR","MLI","MRT","NER","NGA","RWA","SDN","SEN","SLE","SOM","SSD","TCD","TGO","TZA","UGA","ZMB")
country_list_sa=c("ARG","BOL","BRA","COL","ECU","GUF","GUY","PER","PRY","SUR","VEN")

{
  epi_data=readRDS(paste(comp,data_folder,"R0_probs_734regions.Rds",sep="/"))
  regions=epi_data$region
  country_list=names(table(substr(regions,1,3)))
  assertthat::assert_that(all(country_list %in% c(country_list_af,country_list_sa)))
  assertthat::assert_that(all(c(country_list_af,country_list_sa) %in% country_list))
  regions_af=regions[substr(regions,1,3) %in% country_list_af]
  regions_sa=regions[substr(regions,1,3) %in% country_list_sa]
  epi_data_af=subset(epi_data,region %in% regions_af)
  epi_data_sa=subset(epi_data,region %in% regions_sa)
}

{
  shapefiles_af=shapefiles_af_countries=rep("",length(country_list_af))
  for(i in 1:length(country_list_af)){
    shapefiles_af[i]=paste(comp,"/",shapefile_folder,"/",country_list_af[i],"/gadm36_",country_list_af[i],"_1.shp",sep="")
    shapefiles_af_countries[i]=paste(comp,"/",shapefile_folder,"/",country_list_af[i],"/gadm36_",country_list_af[i],"_0.shp",
                                     sep="")
  }
  shape_data_af=map_shapes_load(regions_af, shapefiles_af, region_label_type="GID_1")
  shape_data_af_countries=map_shapes_load(country_list_af, shapefiles_af_countries, region_label_type="GID_0")
}

colour_scheme=readRDS(file=paste(path.package("YEPaux"), "exdata/colour_scheme_example.Rds", sep="/"))
colour_scale=colour_scheme$colour_scale
scale=c(0,1e-5,1e-4,1e-3,1e-2,0.1,0.25,0.5,0.75,0.9,0.99,1.001)

data_types=colnames(epi_data)[c(2:ncol(epi_data))]

for(data_select in 1:length(data_types)){
  data_type=data_types[data_select]
  values_af=epi_data_af[,1+data_select]
  
  create_map(shape_data_af,values_af,scale=scale,colour_scale,pixels_max=1440,text_size=2,
             border_colour_regions="light grey",additional_border_shapes=shape_data_af_countries,
             border_colour_additional="black",legend_title=data_types[data_select],
             legend_position="bottomleft",legend_format="e",legend_dp=1,
             output_file=paste0(comp,"/",results_folder,"/R0_probs_map_af",data_select,".png",sep=""))
}
