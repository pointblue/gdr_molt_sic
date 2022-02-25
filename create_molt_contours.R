# Code to create molt location summary contours


# Check if have required packages installed and install if not
list.of.packages <- c("dplyr","ggplot2","readr","gridExtra","viridis","MASS","data.table","lubridate","fasttime",
                      "wesanderson","sp","raster","rgdal","sf","ggspatial","ggpubr","nngeo")
# compare to existing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install missing packages
if(length(new.packages)>0) {install.packages(new.packages)}
# load required packages
lapply(list.of.packages, library, character.only = TRUE)



# read in MAP layers ####
# set data frame projection
proj <- CRS("+proj=longlat +ellps=WGS84 +lon_0=-169 +lon_wrap")
# proj <- CRS("+proj=laea +lat_0=45.5 +lon_0=-114.125 +no_defs +lon_wrap")
# proj <-CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs" )
proj_ant <-CRS("+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=180 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +datum=WGS84 +units=m +no_defs")
mpa_t <- spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/mpa-shapefile-EPSG102020.shp"),proj_ant)

# read in and project antarctica coastline
ant <- spTransform(readOGR("GIS/ADDcstpoly_edit_2021_11_23.shp"), proj_ant)

# antarctic coastline for clipping
# use geometry to convert from spatialploygons data frame to spatial polygons
ant_clip <- geometry(spTransform(readOGR("GIS/ADDcstpoly_merge_2022-02-22.shp"),proj_ant))

# read in and project grid lines for plotting
polar_grid <-spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/latlong_stereo.shp"), proj_ant)
# read in and project 2000m isobath
iso1000 <- iso1000 <- spTransform(readOGR("GIS/1000_m_isobath.shp"),proj_ant)
# t<-readOGR("GIS/full_2000_m_isobath.shp")
# read in and project ACC boundary
front <-spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/acc_fronts.shp"),proj_ant)




# read in data and format ####
molt_locs <- fread("data/molt_locs500_2022-02-15.csv")

# convert to simple feature
molt_locs_sf <- st_as_sf(molt_locs,coords = c("lon","lat"),
                         crs = "+proj=longlat +ellps=WGS84 +lon_0=-169 +lon_wrap")
# reproject and convert back to data frame
molt_locs_df <- df_spatial(st_transform(molt_locs_sf,proj_ant))

# filter location data to remove dates close to the equinox (after March 27 or day 86)
data=molt_locs_df%>%
  mutate(month=month(time1))%>%
  filter(doy<86)


# Function to summarize molt locations and make figures of summarized data ####
source("code/molt_locs_summary_function.R")

# crozier individual years
croz_17<-  plot_mlocs(data=data,colony=c("CROZ"),seasons=2016,grid_size=50000,legend.title = "Location density",xlab="Longitude", ylab="Latitude",legend.position="none",title="Crozier 2017", 
           rast_path = "data/summary_rasters/croz_molt_locs_2017_filt.tif",
           poly_path = "data/contours/CROZ_molt_locs_2017_filt")
print(croz_17)

croz_18 <- plot_mlocs(data=data,colony=c("CROZ"),seasons=2017,grid_size=50000,legend.title = "",xlab="Longitude", ylab="Latitude",legend.position="none",title="Crozier 2018", 
                      rast_path = "data/summary_rasters/croz_molt_locs_2018_filt.tif",
                      poly_path = "data/contours/CROZ_molt_locs_2018_filt")
print(croz_18)

croz_19 <- plot_mlocs(data=data,colony=c("CROZ"),seasons=2018,grid_size=50000,legend.title = "",xlab="Longitude", ylab="Latitude",legend.position="none",title="Crozier 2019", 
                      rast_path = "data/summary_rasters/croz_molt_locs_2019_filt.tif",
                      poly_path = "data/contours/CROZ_molt_locs_2019_filt")

ggarrange(croz_17,croz_18,croz_19,ncol=1,nrow=3,common.legend = TRUE,legend = "right")


# Royds individual years
royds_17<-  plot_mlocs(data=data,colony=c("ROYD"),seasons=2016,grid_size=50000,legend.title = "",xlab="Longitude", ylab="Latitude",legend.position="none",title="Royds 2017", 
                      rast_path = "data/summary_rasters/royds_molt_locs_2017_filt.tif",
                      poly_path = "data/contours/ROYD_molt_locs_2017_filt")
print(royds_17)

royds_18 <- plot_mlocs(data=data,colony=c("ROYD"),seasons=2017,grid_size=50000,legend.title = "",xlab="Longitude", ylab="Latitude",legend.position="none",title="Royds 2018", 
                      rast_path = "data/summary_rasters/royds_molt_locs_2018_filt.tif",
                      poly_path = "data/contours/ROYD_molt_locs_2018_filt")
print(royds_18)

royds_19 <- plot_mlocs(data=data,colony=c("ROYD"),seasons=2018,grid_size=50000,legend.title = "",xlab="Longitude", ylab="Latitude",legend.position="none",title="Royds 2019", 
                      rast_path = "data/summary_rasters/royds_molt_locs_2019_filt.tif",
                      poly_path = "data/contours/ROYD_molt_locs_2019_filt")

ggarrange(croz_17,croz_18,croz_19,royds_17,royds_18,royds_19,ncol=3,nrow=2,common.legend = TRUE,legend = "right")


cr_all <- plot_mlocs(data=data,colony=c("CROZ","ROYD"),seasons=c(2016:2018),grid_size=50000,legend.title = "",xlab="Longitude", ylab="Latitude",legend.position="none",title="2017-2019", 
                     rast_path = "data/summary_rasters/cr_molt_locs_filt.tif",
                     poly_path = "data/contours/CROZ_ROYD_molt_locs_filt")
print(cr_all)


croz_all <- plot_mlocs(data=data,colony=c("CROZ"),seasons=c(2016:2018),grid_size=50000,legend.title = "Location Density",xlab="Longitude", ylab="Latitude",legend.position="none",title="Crozier 2017-2019", 
                       rast_path = "data/summary_rasters/croz_molt_locs_filt.tif",
                       poly_path = "data/contours/CROZ_molt_locs_filt")

royds_all <- plot_mlocs(data=data,colony=c("ROYD"),seasons=c(2016:2018),grid_size=50000,legend.title = "",xlab="Longitude", ylab="Latitude",legend.position="none",title="Royds 2017-2019", 
                        rast_path = "data/summary_rasters/royd_molt_locs_filt.tif",
                        poly_path = "data/contours/ROYD_molt_locs_filt")

ggarrange(royds_all,croz_all,ncol=2,nrow=1,common.legend = TRUE,legend = "right")



