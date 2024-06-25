library(YEPaux)1
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
  #epi_data=readRDS(paste(comp,data_folder,"FOI_R0_summary_734regions_no_Brazil_multiplier.Rds",sep="/")) 
  epi_data=readRDS(paste(comp,data_folder,"FOI_R0_summary_734regions_alt_no_Brazil_multiplier.Rds",sep="/")) 
  epi_data[,c(3:8)]=epi_data[,c(3:8)]*365.0 #Convert daily FOI to annual
  regions=epi_data$region
  country_list=names(table(substr(regions,1,3)))
  assertthat::assert_that(all(country_list %in% c(country_list_af,country_list_sa)))
  assertthat::assert_that(all(c(country_list_af,country_list_sa) %in% country_list))
  regions_af=regions[substr(regions,1,3) %in% country_list_af]
  regions_sa=regions[substr(regions,1,3) %in% country_list_sa]
  epi_data_af=subset(epi_data,region %in% regions_af)
  epi_data_sa=subset(epi_data,region %in% regions_sa)
}

{ #TODO - Fix Brazil error
  shapefiles_af=rep("",length(country_list_af))
  for(i in 1:length(country_list_af)){
    shapefiles_af[i]=paste(comp,"/",shapefile_folder,"/",country_list_af[i],"/gadm36_",country_list_af[i],"_1.shp",sep="")
  }
  shape_data_af=map_shapes_load(regions_af, shapefiles_af, region_label_type="GID_1")
  shapefiles_sa=rep("",length(country_list_sa))
  for(i in 1:length(country_list_sa)){
    shapefiles_sa[i]=paste(comp,"/",shapefile_folder,"/",country_list_sa[i],"/gadm36_",country_list_sa[i],"_1.shp",sep="")
  }
  shape_data_sa=map_shapes_load(regions_sa, shapefiles_sa, region_label_type="GID_1")
  #Fix Brazil error
  n_regions_sa=length(shape_data_sa$regions)
  shape_data_sa$shapes[[c(1:n_regions_sa)[shape_data_sa$regions=="BRA.4_1"]]][[1]][[2]]=NULL
  shape_data_sa$shapes[[c(1:n_regions_sa)[shape_data_sa$regions=="BRA.23_1"]]][[44]]=NULL
}

colour_scheme=readRDS(file=paste(path.package("YEPaux"), "exdata/colour_scheme_example.Rds", sep="/"))
colour_scale=colour_scheme$colour_scale
scale_FOI=c(0,1e-7,5e-7,1e-6,5e-6,1e-5,2.5e-5,5e-5,1e-4,2e-4,3e-4,4e-4)#pretty(epi_data$FOI_50,10)
scale_R0=c(0,0.5,0.75,1.0,1.1,1.2,1.3,1.4,1.5)#pretty(epi_data$R0_50, 10)

MetBrewer::display.all.met()
palette=MetBrewer::met.brewer("Hiroshige")
hues::swatch(palette)
map_colour_scale_R0=as.vector(palette)[c(10,8,7,5:1)]
assertthat::assert_that(length(map_colour_scale_R0)==length(scale_R0)-1)
hues::swatch(map_colour_scale_R0)

FOI_values_af=epi_data_af[,5]
R0_values_af=epi_data_af[,12]
FOI_values_sa=epi_data_sa[,5]
R0_values_sa=epi_data_sa[,12]

create_map(shape_data_af,FOI_values_af,scale=scale_FOI,colour_scale,pixels_max=1440,
           text_size=1.5,map_title="",legend_title="Spillover force of infection (annual)",legend_position="bottomleft",
           legend_format="e",legend_dp=1,output_file=paste(comp,results_folder,"FOI_map_af.png",sep="/"))
create_map(shape_data_sa,FOI_values_sa,scale=scale_FOI,colour_scale,pixels_max=1440,
           text_size=1.5,map_title="",legend_title="Spillover force of infection (annual)",legend_position="bottomright",
           legend_format="e",legend_dp=1,output_file=paste(comp,results_folder,"/FOI_map_sa.png",sep="/"))

create_map(shape_data_af,R0_values_af,scale=scale_R0,map_colour_scale_R0,pixels_max=1440,
           text_size=2,map_title="",legend_title="Basic reproduction number",legend_position="bottomleft",
           legend_format="f",legend_dp=2,output_file=paste(comp,results_folder,"/R0_map_af.png",sep="/"))
create_map(shape_data_sa,R0_values_sa,scale=scale_R0,map_colour_scale_R0,pixels_max=1440,
           text_size=2,map_title="",legend_title="Basic reproduction number",legend_position="bottomright",
           legend_format="f",legend_dp=2,output_file=paste(comp,results_folder,"/R0_map_sa.png",sep="/"))
