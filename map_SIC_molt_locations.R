# Map of 50% molt location contours with sea ice concentration during molt


# Check if have required packages installed and install if not
list.of.packages <- c("dplyr","ggplot2","readr","gridExtra","viridis","MASS","data.table","lubridate","fasttime",
                      "wesanderson","sp","raster","rgdal","sf","ggspatial")
# compare to existing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install missing packages
if(length(new.packages)>0) {install.packages(new.packages)}
# load required packages
lapply(list.of.packages, library, character.only = TRUE)

# Set working directory
setwd("Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data")


# read in MAP layers ####
# set data frame projection
proj <- CRS("+proj=longlat +ellps=WGS84 +lon_0=-169 +lon_wrap")
# proj <- CRS("+proj=laea +lat_0=45.5 +lon_0=-114.125 +no_defs +lon_wrap")
# proj <-CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs" )
proj_ant <-CRS("+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=180 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +datum=WGS84 +units=m +no_defs")
MPA <- readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/mpa-shapefile-EPSG102020.shp")
# reproject MPA
mpa_t <- spTransform(MPA, proj_ant)

# read in and project antarctica coastline
ant <- spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/ADDcstpoly30.shp"), proj_ant)
# read in and project grid lines for plotting
polar_grid <-spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/latlong_stereo.shp"), proj_ant)
# read in and project 2000m isobath
iso2000 <- spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/2000_m_isobath.shp"),proj_ant)
# t<-readOGR("GIS/full_2000_m_isobath.shp")
# read in and project ACC boundary
front <-spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/acc_fronts.shp"),proj_ant)
# read in 50% contour for molt locations
c_2017_mlocs_50 <- readOGR("contours/CROZ_2017_molt_locs_50_poly.shp")
c_2018_mlocs_50 <- readOGR("contours/CROZ_2018_molt_locs_50_poly.shp")
c_2019_mlocs_50 <- readOGR("contours/CROZ_2019_molt_locs_50_poly.shp")

r_2017_mlocs_50 <-readOGR("contours/ROYD_2017_molt_locs_50_poly.shp")
r_2018_mlocs_50 <-readOGR("contours/ROYD_2018_molt_locs_50_poly.shp")
r_2019_mlocs_50 <-readOGR("contours/ROYD_2019_molt_locs_50_poly.shp")

# Read in 95% contour for molt locations
c_2017_mlocs_95 <- readOGR("contours/CROZ_2017_molt_locs_95.shp")
c_2018_mlocs_95 <- readOGR("contours/CROZ_2018_molt_locs_95.shp")
c_2019_mlocs_95 <- readOGR("contours/CROZ_2019_molt_locs_95.shp")

r_2017_mlocs_95 <-readOGR("contours/ROYD_2017_molt_locs_95.shp")
r_2018_mlocs_95 <-readOGR("contours/ROYD_2018_molt_locs_95.shp")
r_2019_mlocs_95 <-readOGR("contours/ROYD_2019_molt_locs_95.shp")






# Sea ice rasters
sic_2017 <- raster("V:/Project/Terrestrial/adpe/nasa_winter_ecology/ice_concentration/raw/y2017/mean_2017_molt_dates.tif")
sic_2017_t <- as.data.frame(as(projectRaster(sic_2017,crs=proj_ant),"SpatialPixelsDataFrame"))
  
sic_2018 <- raster("V:/Project/Terrestrial/adpe/nasa_winter_ecology/ice_concentration/raw/y2018/mean_2018_molt_dates.tif")
sic_2018_t <- as.data.frame(as(projectRaster(sic_2018,crs=proj_ant),"SpatialPixelsDataFrame"))

  
sic_2019 <- raster("V:/Project/Terrestrial/adpe/nasa_winter_ecology/ice_concentration/raw/y2019/mean_2019_molt_dates.tif")
sic_2019_t <- as.data.frame(as(projectRaster(sic_2019,crs=proj_ant),"SpatialPixelsDataFrame"))

  


# plot molt locations
# 2017####
cr_2017<-ggplot()+
  geom_tile(data=sic_2017_t,aes(x,y,fill=mean_2017_molt_dates))+
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
  geom_path(data=c_2017_mlocs_50,aes(x=long,y=lat, group=group,col="Crozier 50%",linetype="Crozier 50%"),size=1.2)+
  geom_path(data=r_2017_mlocs_50,aes(x=long,y=lat, group=group,col="Royds 50%", linetype="Royds 50%"),size=1.2)+
  # add 95% contours
  geom_path(data=c_2017_mlocs_95,aes(x=long,y=lat, group=group,col="Crozier 95%",linetype="Crozier 95%"),size=.5)+
  geom_path(data=r_2017_mlocs_95,aes(x=long,y=lat, group=group,col="Royds 95%", linetype="Royds 95%"),size=.5)+
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey90",
    # alpha = 0.3,
    col = "grey50")+
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  geom_label(aes(x=-1325000,y=1175000),label="2017")+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1425000,   1600000),
    ylim = c(1105000, 3175000))+
  theme_classic()+
  scale_color_manual("",values=c("gold", "gold","dodgerblue","dodgerblue","grey85","grey50"),
                     breaks = c("Crozier 50%","Crozier 95%", "Royds 50%","Royds 95%","RSRMPA"))+
  scale_linetype_manual(values=c(1,1,1,1,1),
                        breaks = c("Crozier 50%", "Crozier 95%", "Royds 50%","Royds 95%","RSRMPA"),guide=FALSE)+
  guides(col = guide_legend(override.aes = list(linetype = c(1,1,1,1,1),lwd=c(2,0.5,2,0.5,1.2))))+
  theme(title = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(size = 10),
        legend.position = "")+
  labs(x="Longitude",y="Latitude", title = "Crozier core area SIC = 5.4%\nRoyds core area SIC = 5.0%\nRoss Sea SIC = 6.1%") +
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))

print(cr_2017)

# 2018 ####
cr_2018 <- ggplot()+
  geom_tile(data=sic_2018_t,aes(x,y,fill=mean_2018_molt_dates))+
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
  geom_path(data=c_2018_mlocs_50,aes(x=long,y=lat, group=group,col="Crozier 50%",linetype="Crozier 50%"),size=1.2)+
  geom_path(data=r_2018_mlocs_50,aes(x=long,y=lat, group=group,col="Royds 50%", linetype="Royds 50%"),size=1.2)+
  # add 95% contours
  geom_path(data=c_2018_mlocs_95,aes(x=long,y=lat, group=group,col="Crozier 95%",linetype="Crozier 95%"),size=.5)+
  geom_path(data=r_2018_mlocs_95,aes(x=long,y=lat, group=group,col="Royds 95%", linetype="Royds 95%"),size=.5)+
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey90",
    # alpha = 0.3,
    col = "grey50")+
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  geom_label(aes(x=-1325000,y=1175000),label="2018")+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1425000,   1600000),
    ylim = c(1105000, 3175000))+
  theme_classic()+
  scale_color_manual("",values=c("gold", "gold","dodgerblue","dodgerblue","grey85","grey50"),
                     breaks = c("Crozier 50%","Crozier 95%", "Royds 50%","Royds 95%","RSRMPA"))+
  scale_linetype_manual(values=c(1,1,1,1,1),
                        breaks = c("Crozier 50%", "Crozier 95%", "Royds 50%","Royds 95%","RSRMPA"),guide=FALSE)+
  guides(col = guide_legend(override.aes = list(linetype = c(1,1,1,1,1),lwd=c(2,0.5,2,0.5,1.2))))+
  theme(title = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(size = 10),
        legend.position = "")+
  labs(x="Longitude",y="Latitude", title = "Crozier core area SIC = 0.4%\nRoyds core area SIC = 16.9%\nRoss Sea SIC = 5.9%") +
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))

print(cr_2018)


# 2019 ####
cr_2019 <- ggplot()+
  geom_tile(data=sic_2019_t,aes(x,y,fill=mean_2019_molt_dates))+
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
  geom_path(data=c_2019_mlocs_50,aes(x=long,y=lat, group=group,col="Crozier 50%",linetype="Crozier 50%"),size=1.2)+
  geom_path(data=r_2019_mlocs_50,aes(x=long,y=lat, group=group,col="Royds 50%", linetype="Royds 50%"),size=1.2)+
  # add 95% contours
  geom_path(data=c_2019_mlocs_95,aes(x=long,y=lat, group=group,col="Crozier 95%",linetype="Crozier 95%"),size=.5)+
  geom_path(data=r_2019_mlocs_95,aes(x=long,y=lat, group=group,col="Royds 95%", linetype="Royds 95%"),size=.5)+
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey90",
    # alpha = 0.3,
    col = "grey50")+
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  geom_label(aes(x=-1325000,y=1175000),label="2019")+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1425000,   1600000),
    ylim = c(1105000, 3175000))+
  theme_classic()+
  scale_color_manual("",values=c("gold", "gold","dodgerblue","dodgerblue","grey85","grey50"),
                     breaks = c("Crozier 50%","Crozier 95%", "Royds 50%","Royds 95%","RSRMPA"))+
  scale_linetype_manual(values=c(1,1,1,1,1),
                        breaks = c("Crozier 50%", "Crozier 95%", "Royds 50%","Royds 95%","RSRMPA"),guide=FALSE)+
  guides(col = guide_legend(override.aes = list(linetype = c(1,1,1,1,1),lwd=c(2,0.5,2,0.5,1.2))))+
  theme(title = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(size = 10))+
  labs(x="Longitude",y="Latitude", title = "Crozier core area SIC = 12.3%\nRoyds core area SIC = 0.1%\nRoss Sea SIC = 6.6%") +
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))

print(cr_2019)

# print all 3 panels
ggpubr::ggarrange(cr_2017,cr_2018,cr_2019,ncol=3,nrow=1,common.legend = TRUE,legend = "right")
