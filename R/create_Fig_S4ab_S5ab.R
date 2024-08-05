#Create figures 4a-b and 5a-b from main text (maps of spillover FOI and R0 values)

library(YEPaux)
data_folder="Data/Real"
shapefile_folder="C:/Users/kjfras16/Documents/00 - Big data files to back up infrequently/00 - GADM36 shapefiles"
results_folder="Images/Real - main text"

country_list_af=c("AGO","BDI","BEN","BFA","CAF","CIV","CMR","COD","COG","ERI","ETH","GAB","GHA","GIN","GMB","GNB","GNQ",
                  "KEN","LBR","MLI","MRT","NER","NGA","RWA","SDN","SEN","SLE","SOM","SSD","TCD","TGO","TZA","UGA","ZMB")
country_list_sa=c("ARG","BOL","BRA","COL","ECU","GUF","GUY","PER","PRY","SUR","VEN")

{
  epi_data=readRDS(paste0(data_folder,"/FOI_R0_summary_734regions_alt_no_Brazil_multiplier_new.Rds")) 
  epi_data[,2]=epi_data[,2]*365.0 #Convert daily FOI to annual
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
    shapefiles_af[i]=paste0(shapefile_folder,"/",country_list_af[i],"/gadm36_",country_list_af[i],"_1.shp")
    shapefiles_af_countries[i]=paste0(shapefile_folder,"/",country_list_af[i],"/gadm36_",country_list_af[i],"_0.shp")
  }
  shape_data_af=map_shapes_load(regions_af, shapefiles_af, region_label_type="GID_1")
  shape_data_af_countries=map_shapes_load(country_list_af, shapefiles_af_countries, region_label_type="GID_0")
  
  shapefiles_sa=shapefiles_sa_countries=rep("",length(country_list_sa))
  for(i in 1:length(country_list_sa)){
    shapefiles_sa[i]=paste0(shapefile_folder,"/",country_list_sa[i],"/gadm36_",country_list_sa[i],"_1.shp",sep="")
    shapefiles_sa_countries[i]=paste0(shapefile_folder,"/",country_list_sa[i],"/gadm36_",country_list_sa[i],"_0.shp")
  }
  shape_data_sa=map_shapes_load(regions_sa, shapefiles_sa, region_label_type="GID_1")
  shape_data_sa_countries=map_shapes_load(country_list_sa, shapefiles_sa_countries, region_label_type="GID_0")
  #Fix Brazil error
  n_regions_sa=length(shape_data_sa$regions)
  shape_data_sa$shapes[[c(1:n_regions_sa)[shape_data_sa$regions=="BRA.4_1"]]][[1]][[2]]=NULL
  shape_data_sa$shapes[[c(1:n_regions_sa)[shape_data_sa$regions=="BRA.23_1"]]][[44]]=NULL
}

colour_scheme=readRDS(file=paste(path.package("YEPaux"), "exdata/colour_scheme_example.Rds", sep="/"))
colour_scale=colour_scheme$colour_scale
scale_FOI=c(0,5e-7,1e-6,2.5e-6,5e-6,1e-5,2.5e-5,5e-5,1e-4,1.5e-4,2e-4,2.5e-4,3e-4,3.5e-4)#pretty(epi_data$FOI_med_burden,10)
scale_R0=c(0,0.5,0.75,1.0,1.1,1.2,1.3,1.4,1.5) #pretty(epi_data$R0_med_burden, 10)

palette=MetBrewer::met.brewer("Hiroshige")
hues::swatch(palette)
map_colour_scale_R0=as.vector(palette)[c(10,8,7,5:1)]
assertthat::assert_that(length(map_colour_scale_R0)==length(scale_R0)-1)
hues::swatch(map_colour_scale_R0)

FOI_values_af=epi_data_af[,2]
R0_values_af=epi_data_af[,3]
FOI_values_sa=epi_data_sa[,2]
R0_values_sa=epi_data_sa[,3]

create_map(shape_data_af,FOI_values_af,scale=scale_FOI,colour_scale,pixels_max=1440,text_size=1.5,
           border_colour_regions="light grey",additional_border_shapes=shape_data_af_countries,
           border_colour_additional="black",legend_title="Spillover force of infection (annual)",
           legend_position="bottomleft",legend_format="e",legend_dp=1,
           output_file=paste0(results_folder,"/Fig_M4a.png"))
create_map(shape_data_sa,FOI_values_sa,scale=scale_FOI,colour_scale,pixels_max=1440,text_size=1.5,
           lat_max=13,lat_min=-54,long_max=-35,long_min=-81,
           border_colour_regions="light grey",additional_border_shapes=shape_data_sa_countries,
           border_colour_additional="black",legend_title="Spillover force of infection (annual)",
           legend_position="bottomright",legend_format="e",legend_dp=1,
           output_file=paste0(results_folder,"/Fig_M5a.png"))

create_map(shape_data_af,R0_values_af,scale=scale_R0,map_colour_scale_R0,pixels_max=1440,text_size=2,
           border_colour_regions="light grey",additional_border_shapes=shape_data_af_countries,
           border_colour_additional="black",legend_title="Basic reproduction number",legend_position="bottomleft",
           legend_format="f",legend_dp=2,output_file=paste0(results_folder,"/Fig_M4b.png"))
create_map(shape_data_sa,R0_values_sa,scale=scale_R0,map_colour_scale_R0,pixels_max=1440,text_size=2,
           lat_max=13,lat_min=-54,long_max=-35,long_min=-81,
           border_colour_regions="light grey",additional_border_shapes=shape_data_sa_countries,
           border_colour_additional="black",legend_title="Basic reproduction number",legend_position="bottomright",
           legend_format="f",legend_dp=2,output_file=paste0(results_folder,"/Fig_M5b.png"))
