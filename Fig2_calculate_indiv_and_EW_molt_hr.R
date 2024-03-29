# Calculate fraction of individuals from each colony that molt in western or eastern sectors ####

# Check if have required packages installed and install if not
list.of.packages <- c("tidyverse","gridExtra","viridis","MASS","data.table","lubridate","fasttime",
                      "sp","raster","rgdal","sf","ggspatial","nngeo","ggpubr")
# compare to existing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install missing packages
if(length(new.packages)>0) {install.packages(new.packages)}
# load required packages
lapply(list.of.packages, library, character.only = TRUE)


# read in MAP layers if want to plot##
# set data frame projection
# proj <- CRS("+proj=longlat +ellps=WGS84 +lon_0=-169 +lon_wrap")
proj_ant <-
  crs('PROJCS["unnamed",
             GEOGCS["Unknown datum based upon the custom spheroid",
                    DATUM["Not_specified_based_on_custom_spheroid",
                          SPHEROID["Custom_spheroid",6378273,298.279411123064,
                                   AUTHORITY["EPSG","7059"]],
                          AUTHORITY["EPSG","6059"]],
                              PRIMEM["Greenwich",0,            
                              AUTHORITY["EPSG","8901"]],
                              UNIT["degree",0.0174532925199433,            
                              AUTHORITY["EPSG","9122"]],
                              AUTHORITY["EPSG","4059"]],
         PROJECTION["Stereographic"],
         PARAMETER["latitude_of_origin",-90],
         PARAMETER["central_meridian",180],
         PARAMETER["scale_factor",1],
         PARAMETER["false_easting",0],
         PARAMETER["false_northing",0],
         UNIT["metre",1,        
         AUTHORITY["EPSG","9001"]],
         AXIS["X",EAST],
         AXIS["Y",NORTH],
         AUTHORITY["EPSG","3031"]]')

# MPA boundaries
mpa_t <- spTransform(readOGR("GIS/mpa-shapefile-EPSG102020.shp"),proj_ant)

# read in and project antarctica coastline
ant <-spTransform(readOGR("GIS/ADDcstpoly_edit_2021_11_23.shp"), proj_ant)

# ant_t <- spTransform(readOGR("GIS/ADDcstpoly_edit_2021_11_23.shp"), proj_ant)
# antarctic coastline for clipping
# use geometry to convert from spatialploygons data frame to spatial polygons
ant_clip <- geometry(spTransform(readOGR("GIS/ADDcstpoly_merge_2022-02-22.shp"),proj_ant))
# read in and project grid lines for plotting
# polar_grid <-spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/latlong_stereo.shp"), proj_ant)
polar_grid <-spTransform(readOGR("GIS/latlong_stereo.shp"), proj_ant)
# read in and project 1000m isobath
iso1000 <- spTransform(readOGR("GIS/1000_m_isobath.shp"),proj_ant)


# load molt locs data
# read in molt locs from 500 chains
molt_locs <- fread("data/molt_locs500_2022-02-15.csv")%>%
  mutate(doy = as.numeric(format(time1,"%j")))# this table was created with "create_molt_locs500.R" These locations are unprojected
molt_locs_sf <- st_as_sf(molt_locs,coords = c("lon","lat"),
                         crs = "+proj=longlat +dataum = WGS84")
molt_locs_df <- df_spatial(st_transform(molt_locs_sf,proj_ant))

# source function to summarize and plot data
source("code/molt_locs_summary_function.R")


# seas=2017
for (seas in 2017:2019){
  dat <- molt_locs_df%>%
    mutate(month=month(time1),season=year)%>%
    filter(season==seas,doy<86)
  bird_ids <- as.vector(distinct(dat,bird_id))
  for (i in 1:nrow(bird_ids)){
    bid=bird_ids$bird_id[i]
    dat_sub <- filter(dat,bird_id==bid)
    colony=dat_sub$br_col[1]
    seasons=seas
    print(paste(colony,bid,seasons, sep="_"))
    rast_path = paste0("data/summary_rasters/individual_rasters/",colony,"_",bid,"_",seas,".tif")
    poly_path = paste0("data/contours/individual_contours/",colony,"_",bid,"_",seas)
    title=paste(colony,bid,seas, sep="_")
    p <- plot_mlocs(data=dat_sub,colony=colony,seasons,months=unique(dat_sub$month),grid_size=50000,
               rast_path=rast_path,poly_path=poly_path,scaled_rast=TRUE,title=title,legend.title = "",xlab="Longitude",
               ylab="Latitude")
    plot_path = paste0("figs/individual_molt_locs/",colony,"_",bid,"_",seas,"_molt_locs", ".jpg")
    jpeg(file=plot_path,units="in",width=7,height=5, res=150)
    print(p)
    dev.off()
  }
}


# calculated median longitude of molt locations

molt_med_side <- molt_locs%>%
  group_by(year,bird_id,br_col)%>%
  summarise(med_lon = median(lon), mean_lon = mean(lon))%>%
  mutate(med_side = ifelse(med_lon>180,"E","W"))%>%
  group_by(year,br_col,med_side)%>%
  tally()%>%
  pivot_wider(names_from=med_side,values_from = n)%>%
  mutate(W=ifelse(is.na(W),0,W),frac_east = E/(E+W), frac_west=W/(E+W),n=E+W)%>%
  #z*sqrt(p(1-p)/n)
  mutate(fracCI=1.96*sqrt(frac_east*(1-frac_east))/n)

E2017 <- molt_med_side$E[molt_med_side$year==2017]

n2017 <- molt_med_side$n[molt_med_side$year==2017]

prop.test(E2017,n2017, correct = FALSE,alternative = "greater")

E2018 <- molt_med_side$E[molt_med_side$year==2018]

n2018 <- molt_med_side$n[molt_med_side$year==2018]

prop.test(E2018,n2018)

E2019 <- molt_med_side$E[molt_med_side$year==2019]

n2019 <- molt_med_side$n[molt_med_side$year==2019]

prop.test(E2019,n2019)

# figure showing fraction of individuals molting in eastern Ross Sea
col1="#006C84" 
col2="#B2DBD5" # arctic

molt_med_side%>%
  mutate(br_col=factor(br_col))%>%
ggplot(aes(year,frac_east,fill=br_col))+
  geom_col(position="dodge", alpha=0.6, size=1.1)+
  geom_errorbar(aes(ymin=frac_east-fracCI, ymax=frac_east+fracCI), width=.1,
                position=position_dodge(.9), size=0.8)+
  scale_fill_manual(values=c(col1,col2),labels = c("Crozier","Royds"))+
  guides(fill=guide_legend(title="Colony"))+
  theme_classic()+
  labs(x="Year", y="Fraction of individuals that\n molted East of 180?")+
  geom_text(aes(y=0.06,label = c("47/53", "9/12","58/59","10/17","37/37","13/16")),position=position_dodge(0.9),size=3)+
  geom_text(aes(y=0.95,label=c("","","","*","","*")), size=8,position=position_dodge(0.9))



molt_med_side%>%
  group_by(br_col)%>%
  summarise(mean = mean(frac_east))

molt_mean_side <- molt_locs%>%
  group_by(season,bird_id,br_col)%>%
  summarise(mean_lon = mean(lon))%>%
  mutate(mean_side = ifelse(mean_lon>180,"E","W"))%>%
  group_by(season,br_col,mean_side)%>%
  tally()%>%
  pivot_wider(names_from=mean_side,values_from = n)%>%
  mutate(W=ifelse(is.na(W),0,W),frac_east = E/(E+W))

molt_mean_side%>%
  group_by(br_col)%>%
  summarise(mean = mean(frac_east))


# Create molt contours for eastern and western molt areas

molt_sides <- molt_locs%>%
  filter(doy<86)%>%
  group_by(year,bird_id,br_col)%>%
  summarise(med_lon = median(lon), mean_lon = mean(lon))%>%
  mutate(med_side = ifelse(med_lon>180,"E","W"))

# number of individuals that use both locations
molt_sides%>%
  group_by(bird_id,med_side)%>%
  tally()%>%
  pivot_wider(id_cols=bird_id,names_from = med_side,values_from=n)%>%
  mutate(total = sum(E,W,na.rm=TRUE))%>%
  filter(total>1)%>%
  drop_na()

# t <-molt_sides%>%
#   mutate(id = paste(as.character(bird_id),br_col))%>%
#   pivot_wider(id_cols = id,names_from = year,values_from = med_side)

# Eastern molt area
molt_east <-molt_locs_df%>%
  left_join(dplyr::select(molt_sides, year, bird_id,med_side))%>%
  filter(med_side=="E")%>%
  mutate(month=lubridate::month(time1))


east_locs <-plot_mlocs(data=molt_east,
                       colony=c("CROZ","ROYD"),
                       seasons=c(2016,2017,2018),
                       months=unique(molt_east$month),
                       grid_size=50000,
                       rast_path="data/summary_rasters/east_west/east_molt_locs_allyr_filt.tif",
                       poly_path="data/contours/east_west/east_molt_locs_allyr_filt",
                       scaled_rast=TRUE,
                       title="Eastern Molt",
                       legend.title = "Location Density",
                       xlab="Longitude",
                       ylab="Latitude")
print(east_locs)

# Western molt area
molt_west <-molt_locs_df%>%
  left_join(dplyr::select(molt_sides, year, bird_id,med_side))%>%
  filter(med_side=="W")%>%
  mutate(month=lubridate::month(time1))



west_locs <-plot_mlocs(data=molt_west,
                       colony=c("CROZ","ROYD"),
                       seasons=c(2016,2017,2018),
                       months=unique(molt_east$month),
                       grid_size=50000,
                       rast_path="data/summary_rasters/east_west/west_molt_locs_allyr_filt.tif",
                       poly_path="data/contours/east_west/west_molt_locs_allyr_filt",
                       scaled_rast=TRUE,
                       title="Western Molt",
                       legend.title = "Location Density",
                       xlab="Longitude",
                       ylab="Latitude")
print(west_locs)



# plot east and west together
pdf("figs/revision1/Fig2_rev1_v13.pdf", width = 7, height = 3.5)
  ggarrange(west_locs,east_locs,ncol=2,nrow=1,common.legend = TRUE,legend = "right")
dev.off()

 