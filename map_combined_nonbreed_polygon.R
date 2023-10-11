# Map old and new wintering area polygon's together


# Check if have required packages installed and install if not
list.of.packages <- c("dplyr","ggplot2","readr","gridExtra","viridis","MASS","data.table","lubridate","fasttime",
                      "wesanderson","sp","raster","rgdal","sf","ggspatial","ggpattern")
# compare to existing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install missing packages
if(length(new.packages)>0) {install.packages(new.packages)}
# load required packages
lapply(list.of.packages, library, character.only = TRUE)


# define projection
proj_ant <-CRS("+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=180 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +datum=WGS84 +units=m +no_defs")

#  Read in and prepare data ####
old_polygon<-spTransform(readOGR("data/contours/WinterArea_2003_2005/WinterPolygon2003-2005.shp"),proj_ant)

# spTransform(MPA, proj_ant)

# location density raster
summ_SPixDF <-as.data.frame(as(raster("data/summary_rasters/nonbreed_locs_filt.tiff"),"SpatialPixelsDataFrame"))
# 95% contour
cont_95 <- spTransform(readOGR("GIS/CROZ_ROYD_nonbreed_locs_all_95_poly_cst_clip.shp"),proj_ant)


# MPA <- readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/mpa-shapefile-EPSG102020.shp")
MPA <- readOGR("GIS/mpa-shapefile-EPSG102020.shp")
# reproject MPA
mpa_t <- spTransform(MPA, proj_ant)
# read in and project antarctica coastline
# ant <- spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/ADDcstpoly30.shp"), proj_ant)
ant <- spTransform(readOGR("GIS/ADDcstpoly_edit_2021_11_23.shp"), proj_ant)
# read in and project grid lines for plotting
# polar_grid <-spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/latlong_stereo.shp"), proj_ant)
polar_grid <-spTransform(readOGR("GIS/latlong_stereo.shp"), proj_ant)
# read in and project 2000m isobath
# iso2000 <- spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/2000_m_isobath.shp"),proj_ant)
iso1000 <- spTransform(readOGR("GIS/1000_m_isobath.shp"),proj_ant)
# t<-readOGR("GIS/full_2000_m_isobath.shp")
# read in and project ACC boundary
# front <-spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/acc_fronts.shp"),proj_ant)
front <-spTransform(readOGR("GIS/acc_fronts.shp"),proj_ant)

# combined polygon
comb_poly <- spTransform(readOGR("data/contours/WinterPolygon2003-05_2017-19.shp"),proj_ant)


legend.title= "2017-2019\nLocation Density"
xlab="Longitude"
ylab="Latitude"
prev_study = "2004-2005\nPrevious Study"

p1 <- ggplot()+
  geom_tile(data=summ_SPixDF,aes(x,y,fill=nonbreed_locs_filt))+
  scale_fill_viridis(legend.title)+
  # mpa boundary
  # geom_polygon(data=mpa_t,aes(x=long,y=lat, group=group,col="RSRMPA"),
  #              show.legend = "line",fill="grey",alpha=0,size=0.7)+
  geom_path(data=cont_95,aes(x=long,y=lat,group=group,col="2017-2019\n95%"), show.legend = TRUE,size=0.8)+
  # old polygon
  geom_polygon(data=old_polygon,aes(x=long,y=lat, group=group,col="2004-2005\nPrevious Study"),
               show.legend = "line",fill="grey",alpha=0,size=0.8)+
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey90",
    # alpha = 0.3,
    col = "grey50",
    size=0.85
  ) +
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1625000,   2575000),
    ylim = c(825000, 4175000),
  )+
  theme_classic()+ 
  scale_color_manual("",values=c("2017-2019\n95%"="purple", "2004-2005\nPrevious Study" = "yellow","Combined" = "dodgerblue3" ), #"1000m isobath" = "grey50"
                     breaks = c("2017-2019\n95%","2004-2005\nPrevious Study","Combined"))+
  # scale_linetype_manual(values =c("95%"=1,"RSRMPA" = 1,"1000m isobath" = 1, "Prev Study" =1))+
  guides(color = guide_legend(override.aes = list(linetype = c(1,1,1))))+
  scale_linetype(guide = FALSE)+
  theme(title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(size = 10))+
  xlab(xlab) +
  ylab(ylab) +
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))

print(p1)

p2 <- ggplot()+
  geom_tile(data=summ_SPixDF,aes(x,y,fill=nonbreed_locs_filt))+
  scale_fill_viridis(legend.title)+
  # mpa boundary
  # geom_polygon(data=mpa_t,aes(x=long,y=lat, group=group,col="RSRMPA"),show.legend = "line",fill="grey",alpha=0,size=0.7)+
  geom_path(data=comb_poly,aes(x=long,y=lat,group=group,col="Combined"), show.legend = TRUE,size=0.9)+
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey90",
    # alpha = 0.3,
    col = "grey50",
    size=0.85
  ) +
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1625000,   2575000),
    ylim = c(825000, 4175000),
  )+
  theme_classic()+ 
  scale_color_manual("",values=c("Combined"="dodgerblue3"),
                     breaks = c("Combined"))+
  guides(color = guide_legend(override.aes = list(linetype = c(1))))+
  scale_linetype(guide = FALSE)+
  theme(title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(size = 10))+
  xlab(xlab) +
  ylab(ylab) +
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))
print(p2)

ggpubr::ggarrange(p1,p2,ncol=2,nrow=1,common.legend = TRUE,legend = "right")
