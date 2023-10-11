# Map of all locations
# filtered to remove the 25% worst positions around the equinox
# between day 86-109 and 229-269


# Check if have required packages installed and install if not
list.of.packages <- c("dplyr","ggplot2","readr","gridExtra","viridis","MASS","data.table","lubridate","fasttime",
                      "wesanderson","sp","raster","rgdal","sf","ggspatial")
# compare to existing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install missing packages
if(length(new.packages)>0) {install.packages(new.packages)}
# load required packages
lapply(list.of.packages, library, character.only = TRUE)


# read in and prepare location data
# 500 chains
locs500<- read_csv("data/gdr_locs_final_500_all_yr_v2021-06-03.csv")%>%
    mutate(doy=as.numeric(format(time1,"%j")))%>%
  filter(!doy%in%c(86:109)&!doy%in%c(229:269))%>%
    mutate(year=factor(season+1),month=month(time1))


# read in GDR deploy file to filter out birds with incomplete tracks
gdr_depl <- read_csv("data/croz_royds_gdr_depl_all_v2021-08-27.csv")

# filter to files that are complete
locs500_depl <-locs500%>%
  left_join(dplyr::select(gdr_depl,bird_fn,season,data,br_col))%>%
  filter(data=="c")

# check how many deployments and unique IDs
# unique IDs
nrow(distinct(locs500_depl,bird_fn)) # 111

locs500_depl%>%
  group_by(bird_fn,season)%>%
  dplyr::select(bird_fn,season)%>%
  distinct() # 185 deployments with complete data

nrow(locs500_depl)/500
# 62715 total locations

# clear environment
rm(list = ls())
# restart R session to clear memory...

# reload packages
list.of.packages <- c("dplyr","ggplot2","readr","gridExtra","viridis","MASS","data.table","lubridate","fasttime",
                      "wesanderson","sp","raster","rgdal","sf","ggspatial")
# load required packages
lapply(list.of.packages, library, character.only = TRUE)

# set data frame projection
proj <- CRS("+proj=longlat +ellps=WGS84 +lon_0=-169 +lon_wrap")
proj_ant <-CRS("+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=180 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")

# read in MAP layers ####
MPA <- readOGR("GIS/mpa-shapefile-EPSG102020.shp")
# reproject MPA
mpa_t <- spTransform(MPA, proj_ant)
# read in and project antarctica coastline
ant <- spTransform(readOGR("GIS/ADDcstpoly_edit_2021_11_23.shp"), proj_ant)
# read in and project grid lines for plotting
polar_grid <-spTransform(readOGR("GIS/latlong_stereo.shp"), proj_ant)
# read in and project 1000m isobath
iso1000 <- spTransform(readOGR("GIS/1000_m_isobath.shp"),proj_ant)
# read in and project ACC boundary
front <-spTransform(readOGR("GIS/acc_fronts.shp"),proj_ant)


# Function to summarize molt locations and make figures of summarized data ####
source("code/molt_locs_summary_function.R")

cr_all <- plot_mlocs(data=locsall,colony=c("CROZ","ROYD"),seasons=c(2016:2018),grid_size=50000,
                     legend.title = "Location\nDensity",xlab="Longitude", ylab="Latitude",
                     legend.position="none",
                     title="",
                     rast_path = "data/summary_rasters/nonbreed_locs_filt_17_19.tiff",
                     poly_path = "data/contours/nonbreed_locs_filt_17_19")
print(cr_all)


