# Map of 50% molt location contours with sea ice concentration during molt
# modified by AS 3/9/2022

# Check if have required packages installed and install if not
list.of.packages <- c("dplyr","ggplot2","readr","gridExtra","viridis","MASS","data.table","lubridate","fasttime",
                      "wesanderson","sp","raster","rgdal","sf","ggspatial","nngeo", "ggpubr", "patchwork")
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
mpa_t <- spTransform(readOGR("GIS/mpa-shapefile-EPSG102020.shp"),proj_ant)

# read in and project antarctica coastline
ant <- spTransform(readOGR("GIS/ADDcstpoly_edit_2021_11_23.shp"), proj_ant)

# antarctic coastline for clipping
# use geometry to convert from spatialploygons data frame to spatial polygons
ant_clip <- geometry(spTransform(readOGR("GIS/ADDcstpoly_merge_2022-02-22.shp"),proj_ant))


# read in and project grid lines for plotting
polar_grid <-spTransform(readOGR("GIS/latlong_stereo.shp"), proj_ant)
# read in and project 2000m isobath
iso1000 <- spTransform(readOGR("GIS/1000_m_isobath.shp"),proj_ant)

# load molt contours by year and re-project to SIC projection
molt_locs_dir<-paste0("data/contours")

# Read in east and west contours for each year
east50_2017 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_2017_filt_50_poly.shp")),proj_ant)
east50_2018 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_2018_filt_50_poly.shp")),proj_ant)
east50_2019 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_2019_filt_50_poly.shp")),proj_ant)

east95_2017 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_2017_filt_95_poly.shp")),proj_ant)
east95_2018 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_2018_filt_95_poly.shp")),proj_ant)
east95_2019 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_2019_filt_95_poly.shp")),proj_ant)

west50_2017 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_2017_filt_50_poly.shp")),proj_ant)
west50_2018 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_2018_filt_50_poly.shp")),proj_ant)
west50_2019 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_2019_filt_50_poly.shp")),proj_ant)

west95_2017 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_2017_filt_95_poly.shp")),proj_ant)
west95_2018 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_2018_filt_95_poly.shp")),proj_ant)
west95_2019 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_2019_filt_95_poly.shp")),proj_ant)

# contours with all 3 yr combined
east50 <- spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_allyr_filt_50_poly.shp")), proj_ant)
east95 <- spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_allyr_filt_95_poly.shp")), proj_ant)
west50 <- spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_allyr_filt_50_poly.shp")), proj_ant)
west95 <- spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_allyr_filt_95_poly.shp")), proj_ant)


# combined polygon
comb_poly <- spTransform(readOGR("data/contours/WinterPolygon2003-05_2017-19.shp"),proj_ant)



# Sea ice rasters
sic_2017 <- raster("data/sic/ssmi_2017-2019/mean_ant_sic_ssmi_2017_molt_dates.tif")
sic_2017_t <- as.data.frame(as(projectRaster(sic_2017,crs=proj_ant),"SpatialPixelsDataFrame"))
  
sic_2018 <- raster("data/sic/ssmi_2017-2019/mean_ant_sic_ssmi_2018_molt_dates.tif")
sic_2018_t <- as.data.frame(as(projectRaster(sic_2018,crs=proj_ant),"SpatialPixelsDataFrame"))

  
sic_2019 <- raster("data/sic/ssmi_2017-2019/mean_ant_sic_ssmi_2019_molt_dates.tif")
sic_2019_t <- as.data.frame(as(projectRaster(sic_2019,crs=proj_ant),"SpatialPixelsDataFrame"))

sic_2003 <- raster("data/sic/ssmi_2003_2006/mean_ant_sic_ssmi_2003_molt_dates.tif")
sic_2003_t <- as.data.frame(as(projectRaster(sic_2003,crs=proj_ant),"SpatialPixelsDataFrame"))
  
sic_2006 <- raster("data/sic/ssmi_2003_2006/mean_ant_sic_ssmi_2006_molt_dates.tif")
sic_2006_t <- as.data.frame(as(projectRaster(sic_2006,crs=proj_ant),"SpatialPixelsDataFrame"))



# plot molt locations
# define colors
col1 <- "#FDE725FF" #yellow
col2 <- "#9856c8" # light purple
col4 <- "#440154FF" # dark purple
col3 <- "#21908CFF" # turquoise

custom_theme <-
  function(){
  theme(title = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 0),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        legend.spacing.y = unit(1, "mm"),
        plot.margin = unit(c(0.1,0.2,0.1,0.2), "cm"))
  }
  

# 2017####
ew_2017<-
  ggplot()+
  geom_tile(data=sic_2017_t,aes(x,y,fill=mean_ant_sic_ssmi_2017_molt_dates))+
  # scale_fill_grey("SIC")
  scale_fill_gradient(
  low = "#000000",
  high = "#FFFFFF",
  "SIC (%)")+
  # 2000m isobath
  # geom_path(data=iso2000,aes(x = long, y = lat,group=group,col="2000m isobath",linetype="2000m isobath"),size=0.75)+
  # mpa boundary
  geom_path(data=mpa_t,aes(x=long,y=lat, group=group,col="RSRMPA",linetype="RSRMPA"),size=0.75)+
  # add 50% contours
  geom_path(data=east50_2017,aes(x=long,y=lat, group=group,col="East 50%",linetype="East 50%"),size=1.2)+
  geom_path(data=west50_2017,aes(x=long,y=lat, group=group,col="West 50%", linetype="West 50%"),size=1.2)+
  # add 95% contours
  geom_path(data=east95_2017,aes(x=long,y=lat, group=group,col="East 95%",linetype="East 95%"),size=.5)+
  geom_path(data=west95_2017,aes(x=long,y=lat, group=group,col="West 95%", linetype="West 95%"),size=.5)+
  # combined HR
  geom_path(data=comb_poly,aes(x=long,y=lat, group=group,col="Combined\nNonbreeding\nRange",linetype="Combined\nNonbreeding\nRange"),size=1.2)+
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey75",
    # alpha = 0.3,
    col = "grey50")+
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  geom_label(aes(x=-1325000,y=1175000),label="2017",hjust = "inward", size = 2.5)+
  geom_text(aes(x=-1325000,y=3005000),label="C",color="white",size=6)+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1425000,   1600000),
    ylim = c(1105000, 3175000))+
  theme_classic()+
  scale_color_manual(name="", values=c(col1, col1,col2,col2,col3,"grey85","grey50"),
                     breaks = c("East 50%","East 95%", "West 50%","West 95%","Combined\nNonbreeding\nRange", "RSRMPA"))+
  scale_linetype_manual(values=c(1,1,1,1,1,1),
                        breaks = c("East 50%","East 95%", "West 50%","West 95%", "Combined\nNonbreeding\nRange","RSRMPA"),guide="none")+
  # guides(col = guide_legend(override.aes = list(linetype = c(1,1,1,1,1,1),lwd=c(2,0.5,2,0.5,2,1.2))))+
  custom_theme()+
  theme(
        legend.position = "")+
  labs(x="",y="Latitude") +
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))

# print(ew_2017)

# 2018 ####
ew_2018<-
  ggplot()+
  geom_tile(data=sic_2018_t,aes(x,y,fill=mean_ant_sic_ssmi_2018_molt_dates))+
  # scale_fill_grey("SIC")
  scale_fill_gradient(
    low = "#000000",
    high = "#FFFFFF",
    "SIC (%)")+
  # 2000m isobath
  # geom_path(data=iso2000,aes(x = long, y = lat,group=group,col="2000m isobath",linetype="2000m isobath"),size=0.75)+
  # mpa boundary
  geom_path(data=mpa_t,aes(x=long,y=lat, group=group,col="RSRMPA",linetype="RSRMPA"),size=0.75)+
  # add 50% contours
  geom_path(data=east50_2018,aes(x=long,y=lat, group=group,col="East 50%",linetype="East 50%"),size=1.2)+
  geom_path(data=west50_2018,aes(x=long,y=lat, group=group,col="West 50%", linetype="West 50%"),size=1.2)+
  # add 95% contours
  geom_path(data=east95_2018,aes(x=long,y=lat, group=group,col="East 95%",linetype="East 95%"),size=.5)+
  geom_path(data=west95_2018,aes(x=long,y=lat, group=group,col="West 95%", linetype="West 95%"),size=.5)+
  # combined HR
  geom_path(data=comb_poly,aes(x=long,y=lat, group=group,col="Combined\nNonbreeding\nRange",linetype="Combined\nNonbreeding\nRange"),size=1.2)+
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey75",
    # alpha = 0.3,
    col = "grey50")+
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  geom_label(aes(x=-1325000,y=1175000),label="2018", hjust = "inward", size = 2.5)+
  geom_text(aes(x=-1325000,y=3005000),label="D",color="white",size=6)+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1425000,   1600000),
    ylim = c(1105000, 3175000))+
  theme_classic()+
  scale_color_manual(name="", values=c(col1, col1,col2,col2,col3,"grey85","grey50"),
                     breaks = c("East 50%","East 95%", "West 50%","West 95%","Combined\nNonbreeding\nRange", "RSRMPA"))+
  scale_linetype_manual(values=c(1,1,1,1,1,1),
                        breaks = c("East 50%","East 95%", "West 50%","West 95%", "Combined\nNonbreeding\nRange","RSRMPA"),guide="none")+
  custom_theme() +
  theme(
        legend.position = "")+
  labs(x="",y="") +
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))

# print(ew_2018)

# 2019 ####
ew_2019<-
  ggplot()+
  geom_tile(data=sic_2019_t,aes(x,y,fill=mean_ant_sic_ssmi_2019_molt_dates))+
  # scale_fill_grey("SIC")
  scale_fill_gradient(
    low = "#000000",
    high = "#FFFFFF",
    "SIC (%)")+
  # 2000m isobath
  # geom_path(data=iso2000,aes(x = long, y = lat,group=group,col="2000m isobath",linetype="2000m isobath"),size=0.75)+
  # mpa boundary
  geom_path(data=mpa_t,aes(x=long,y=lat, group=group,col="RSRMPA",linetype="RSRMPA"),size=0.75)+
  # add 50% contours
  geom_path(data=east50_2019,aes(x=long,y=lat, group=group,col="East 50%",linetype="East 50%"),size=1.2)+
  geom_path(data=west50_2019,aes(x=long,y=lat, group=group,col="West 50%", linetype="West 50%"),size=1.2)+
  # add 95% contours
  geom_path(data=east95_2019,aes(x=long,y=lat, group=group,col="East 95%",linetype="East 95%"),size=.5)+
  geom_path(data=west95_2019,aes(x=long,y=lat, group=group,col="West 95%", linetype="West 95%"),size=.5)+
  # combined HR
  geom_path(data=comb_poly,aes(x=long,y=lat, group=group,col="Combined\nNonbreeding\nRange",linetype="Combined\nNonbreeding\nRange"),size=1.2)+
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey75",
    # alpha = 0.3,
    col = "grey50")+
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  geom_text(aes(x=-1325000,y=3005000),label="E",color="white",size=6)+
  geom_label(aes(x=-1325000,y=1175000),label="2019", hjust = "inward", size = 2.5)+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1425000,   1600000),
    ylim = c(1105000, 3175000))+
  theme_classic()+
  scale_color_manual(name="", values=c(col1, col1,col2,col2,col3,"grey85","grey50"),
                     breaks = c("East 50%","East 95%", "West 50%","West 95%","Combined\nNonbreeding\nRange", "RSRMPA"))+
  scale_linetype_manual(values=c(1,1,1,1,1,1),
                        breaks = c("East 50%","East 95%", "West 50%","West 95%", "Combined\nNonbreeding\nRange","RSRMPA"),guide="none")+
  guides(col = guide_legend(override.aes = list(linetype = c(1,1,1,1,1,1),lwd=c(2,0.5,2,0.5,2,1.2))))+
  custom_theme() +
  theme(
    legend.position = "")+
  labs(x="Longitude",y="") +
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))
# print(ew_2019)

# 2003 ####
ew_2003<-
  ggplot()+
  geom_tile(data=sic_2003_t,aes(x,y,fill=mean_ant_sic_ssmi_2003_molt_dates))+
  # scale_fill_grey("SIC")
  scale_fill_gradient(
    low = "#000000",
    high = "#FFFFFF",
    "SIC (%)")+
  # 2000m isobath
  # geom_path(data=iso2000,aes(x = long, y = lat,group=group,col="2000m isobath",linetype="2000m isobath"),size=0.75)+
  # mpa boundary
  geom_path(data=mpa_t,aes(x=long,y=lat, group=group,col="RSRMPA",linetype="RSRMPA"),size=0.75)+
  # add 50% contours
  geom_path(data=east50,aes(x=long,y=lat, group=group,col="East 50%",linetype="East 50%"),size=1.2)+
  geom_path(data=west50,aes(x=long,y=lat, group=group,col="West 50%", linetype="West 50%"),size=1.2)+
  # add 95% contours
  geom_path(data=east95,aes(x=long,y=lat, group=group,col="East 95%",linetype="East 95%"),size=.5)+
  geom_path(data=west95,aes(x=long,y=lat, group=group,col="West 95%", linetype="West 95%"),size=.5)+
  # combined HR
  geom_path(data=comb_poly,aes(x=long,y=lat, group=group,col="Combined\nNonbreeding\nRange",linetype="Combined\nNonbreeding\nRange"),size=1.2)+
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey75",
    # alpha = 0.3,
    col = "grey50")+
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  geom_text(aes(x=-1325000,y=3005000),label="A",color="white",size=6)+
  geom_label(aes(x=-1325000,y=1175000),label="High SIC (2003)", hjust = "inward", size = 2.5)+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1425000,   1600000),
    ylim = c(1105000, 3175000))+
  theme_classic()+
  scale_color_manual(name="", values=c(col1, col1,col2,col2,col3,"grey85","grey50"),
                     breaks = c("East 50%","East 95%", "West 50%","West 95%","Combined\nNonbreeding\nRange", "RSRMPA"))+
  scale_linetype_manual(values=c(1,1,1,1,1,1),
                        breaks = c("East 50%","East 95%", "West 50%","West 95%", "Combined\nNonbreeding\nRange","RSRMPA"),guide="none")+
  # guides(col = guide_legend(override.aes = list(linetype = c(1,1,1,1,1,1),lwd=c(2,0.5,2,0.5,2,1.2))))+
  custom_theme() +
  theme(
    legend.position = "")+
  labs(x="",y="") +
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))

# print(ew_2003)

# 2006####
ew_2006<-
  ggplot()+
  geom_tile(data=sic_2006_t,aes(x,y,fill=mean_ant_sic_ssmi_2006_molt_dates))+
  # scale_fill_grey("SIC")
  scale_fill_gradient(
    low = "#000000",
    high = "#FFFFFF",
    "SIC (%)")+
  # 2000m isobath
  # geom_path(data=iso2000,aes(x = long, y = lat,group=group,col="2000m isobath",linetype="2000m isobath"),size=0.75)+
  # mpa boundary
  geom_path(data=mpa_t,aes(x=long,y=lat, group=group,col="RSRMPA",linetype="RSRMPA"),size=0.75)+
  # add 50% contours
  geom_path(data=east50,aes(x=long,y=lat, group=group,col="East 50%",linetype="East 50%"),size=1.2)+
  geom_path(data=west50,aes(x=long,y=lat, group=group,col="West 50%", linetype="West 50%"),size=1.2)+
  # add 95% contours
  geom_path(data=east95,aes(x=long,y=lat, group=group,col="East 95%",linetype="East 95%"),size=.5)+
  geom_path(data=west95,aes(x=long,y=lat, group=group,col="West 95%", linetype="West 95%"),size=.5)+
  # combined HR
  geom_path(data=comb_poly,aes(x=long,y=lat, group=group,col="Combined\nNonbreeding\nRange",linetype="Combined\nNonbreeding\nRange"),size=1.2)+
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey75",
    # alpha = 0.3,
    col = "grey50")+
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  geom_text(aes(x=-1325000,y=3005000),label="B",color="white",size=6)+
  geom_label(aes(x=-1325000,y=1175000),label="Average SIC (2006)", hjust = "inward", size = 2.5)+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1425000,   1600000),
    ylim = c(1105000, 3175000))+
  theme_classic()+
  scale_color_manual(name="", values=c(col1, col1,col2,col2,col3,"grey85","grey50"),
                     breaks = c("East 50%","East 95%", "West 50%","West 95%","Combined\nNonbreeding\nRange", "RSRMPA"))+
  scale_linetype_manual(values=c(1,1,1,1,1,1),
                        breaks = c("East 50%","East 95%", "West 50%","West 95%", "Combined\nNonbreeding\nRange","RSRMPA"),guide="none")+
  guides(col = guide_legend(override.aes = list(linetype = c(1,1,1,1,1,1),lwd=c(2,0.5,2,0.5,2,1.2))))+
  custom_theme() +
  theme(
    legend.position = "")+
  labs(x="",y= "") +
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))




# use 2006 map to get legend ####
ew_2006_leg<-
  ggplot()+
  geom_tile(data=sic_2006_t,aes(x,y,fill=mean_ant_sic_ssmi_2006_molt_dates))+
  # scale_fill_grey("SIC")
 
  # 2000m isobath
  # geom_path(data=iso2000,aes(x = long, y = lat,group=group,col="2000m isobath",linetype="2000m isobath"),size=0.75)+
  # mpa boundary
  geom_path(data=mpa_t,aes(x=long,y=lat, group=group,col="RSRMPA",linetype="RSRMPA"),size=0.75)+
  # add 50% contours
  geom_path(data=east50,aes(x=long,y=lat, group=group,col="East 50%",linetype="East 50%"),size=1.2)+
  geom_path(data=west50,aes(x=long,y=lat, group=group,col="West 50%", linetype="West 50%"),size=1.2)+
  # add 95% contours
  geom_path(data=east95,aes(x=long,y=lat, group=group,col="East 95%",linetype="East 95%"),size=.5)+
  geom_path(data=west95,aes(x=long,y=lat, group=group,col="West 95%", linetype="West 95%"),size=.5)+
  # combined HR
  geom_path(data=comb_poly,aes(x=long,y=lat, group=group,col="Combined\nNonbreeding\nRange",linetype="Combined\nNonbreeding\nRange"),size=1.2)+
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey75",
    # alpha = 0.3,
    col = "grey50")+
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  geom_text(aes(x=-1325000,y=3005000),label="B",color="white",size=6)+
  geom_label(aes(x=-1100000,y=1175000),label="Average SIC (2006)", hjust = "inward", size = 3)+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1425000,   1600000),
    ylim = c(1105000, 3175000))+
  theme_classic()+
  scale_color_manual(name="", values=c(col1, col1,col2,col2,col3,"grey85","grey50"),
                     breaks = c("East 50%","East 95%", "West 50%","West 95%","Combined\nNonbreeding\nRange", "RSRMPA"))+
  scale_linetype_manual(values=c(1,1,1,1,1,1),
                        breaks = c("East 50%","East 95%", "West 50%","West 95%", "Combined\nNonbreeding\nRange","RSRMPA"),guide="none")+
  scale_fill_gradient(
    low = "#000000",
    high = "#FFFFFF",
    "SIC (%)")+
  guides(col = guide_legend(override.aes = list(linetype = c(1,1,1,1,1,1),lwd=c(1.2,0.5,1.2,0.5,1.2,0.75)))) +
  custom_theme() +
  theme(plot.margin = unit(c(3,3,6,3), "cm"))+
  labs(x="",y= "") +
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))

# print(ew_2006_leg)
leg <- ggpubr::get_legend(ew_2006_leg)
leg$layout[1,] <- c(5,4,2,4,2, "off", "guides")
leg$layout[2,] <-
  c(5,1,1,1,1,"off","guides")

plot(leg)



# print all 3 panels ####
# ggarrange(ew_2003, ew_2006, ew_2017,ew_2018,ew_2019,common.legend = TRUE, ncol=2,nrow=3)
# using patchwork package for multi-panel layout

pdf("figs/revision1/Fig4_SIC_ssmi_molt_xyr_maps_v27.pdf",height=7.75,width= 7)
ew_2003 + ew_2006 + ew_2017 + ew_2018 + ew_2019+ leg+ 
  plot_layout(widths = 1,nrow = 3)
dev.off()

# jpeg("figs/revision1/Fig4_SIC_ssmi_molt_xyr_maps.jpg",height=11,width=8.5,unit="in",res=300)
# ew_2003 + ew_2006 + ew_2017 + ew_2018 + ew_2019+ leg+
#   plot_layout(widths = 1,nrow = 3)
# dev.off()
