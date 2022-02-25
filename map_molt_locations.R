# Map of molt locations


# Check if have required packages installed and install if not
list.of.packages <- c("dplyr","ggplot2","readr","gridExtra","viridis","MASS","data.table","lubridate","fasttime",
                      "wesanderson","sp","raster","rgdal","sf","ggspatial")
# compare to existing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install missing packages
if(length(new.packages)>0) {install.packages(new.packages)}
# load required packages
lapply(list.of.packages, library, character.only = TRUE)

setwd("Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date")
# # read in and prepare molt locations
# molt_locs <- read_csv("Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/molt_end_locs.csv")%>%
#   filter(!is.na(lat50_mend))
# 
# molt_locs_sf <- st_as_sf(molt_locs,coords = c("lon50_mend","lat50_mend"),
#                          crs = "+proj=longlat +ellps=WGS84 +lon_0=-169 +lon_wrap")%>%
#   # only keep one loc per day
#   group_by(season,bird_id)%>%
#   slice(1)


# read in MAP layers ####
# set data frame projection
proj <- CRS("+proj=longlat +ellps=WGS84 +lon_0=-169 +lon_wrap")
# proj <- CRS("+proj=laea +lat_0=45.5 +lon_0=-114.125 +no_defs +lon_wrap")
# proj <-CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs" )
proj_ant <-CRS("+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=180 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
# MPA <- readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/mpa-shapefile-EPSG102020.shp")
MPA <- readOGR("../nonbreeding_foraging/GIS/mpa-shapefile-EPSG102020.shp")
# reproject MPA
mpa_t <- spTransform(MPA, proj_ant)

# read in and project antarctica coastline
# ant <- spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/ADDcstpoly30.shp"), proj_ant)
ant <- spTransform(readOGR("../nonbreeding_foraging/GIS/ADDcstpoly30.shp"), proj_ant)
# read in and project grid lines for plotting
# polar_grid <-spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/latlong_stereo.shp"), proj_ant)
polar_grid <-spTransform(readOGR("../nonbreeding_foraging/GIS/latlong_stereo.shp"), proj_ant)
# read in and project 2000m isobath
# iso2000 <- spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/2000_m_isobath.shp"),proj_ant)
iso2000 <- spTransform(readOGR("../nonbreeding_foraging/GIS/2000_m_isobath.shp"),proj_ant)
# t<-readOGR("GIS/full_2000_m_isobath.shp")
# read in and project ACC boundary
# front <-spTransform(readOGR("Z:/Informatics/S031/analyses/nonbreeding_foraging/GIS/acc_fronts.shp"),proj_ant)
front <-spTransform(readOGR("../nonbreeding_foraging/GIS/acc_fronts.shp"),proj_ant)



# plot molt locations
ggplot()+
  # geom_tile(data=summ_SPixDF,aes(x,y,fill=layer))+
  geom_sf(data=molt_locs_sf,aes(col=br_col))+
  # scale_colour_viridis("Breeding Colony")+
  facet_wrap(~season)+
  # 2000m isobath
  geom_path(data=iso2000,aes(x = long, y = lat,group=group),col="grey50",size=0.75)+
  # acc front
  geom_path(data=front,aes(x = long, y = lat,group=group),col="grey60",size=0.8,lty=3)+
  # add 50% and 95% contours
  # geom_sf(data=cont_50,color="green",size=1)+
  # geom_sf(data=cont_95,size=1.2,color="purple")+
  # mpa boundary
  geom_polygon(data=mpa_t,aes(x=long,y=lat, group=group),fill="grey",alpha=0,col="grey70",size=0.8)+
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey90",
    # alpha = 0.3,
    col = "grey50"
  ) +
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1625000,   2575000),
    ylim = c(825000, 4175000),
  ) +
  theme_classic()+
  theme(title = element_text(size = 14),
         axis.title = element_text(size = 14),
         axis.text = element_text(size = 8),
         axis.text.x = element_text(angle = 90),
         legend.title = element_text(size= 14))+
  # scale_color_manual(values=c(", "blue"), 
  #                    labels=c("route 1", "route 2"))
  xlab("Longitude") +
  ylab("Latitude") +
  # scale_fill_manual(lims=c(0.0001,1))+
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))
  # ggtitle(title)
# c(seq(110,180,by=10),seq(-170,-100,by=10)))
# 


# Map using 500 chains

# gdr_depl <- read_csv("Z:/Informatics/S031/analyses/GDR/data/croz_royds_gdr_depl_all_v2021-08-27.csv")
gdr_depl <- read_csv("Y:/S031/analyses/GDR/data/croz_royds_gdr_depl_all_v2021-08-27.csv")
# locs500 <- read_csv("Z:/Informatics/S031/analyses/GDR/data/gdr_locs_final_500_all_yr_v2021-06-03.csv")
locs500 <- read_csv("Y:/S031/analyses/GDR/data/gdr_locs_final_500_all_yr_v2021-06-03.csv")

# Table with molt and arrival dates estimated from GDR data
# molt_tab<- read_csv("Z:/Informatics/S031/analyses/GDR/data/gdr_molt_arrival_dates_v2021-06-03.csv")
molt_tab<- read_csv("Y:/S031/analyses/GDR/data/gdr_molt_arrival_dates_v2021-06-03.csv")


# join molt dates to gdr_depl
gdr_molt <-gdr_depl%>%
  left_join(molt_tab)%>%
  filter(!is.na(molt_dur))%>%
  # filter(sex!="U",study=="KA")%>% # think I don't want to actually filter out the U's because they are mostly from Royds
  # add molt DOY column
  mutate(mstart_doy=as.numeric(format(start_molt, "%j")),
         mend_doy = as.numeric(format(end_molt, "%j")))%>%
  mutate(breed_cat = factor(ifelse(breeder == 0,1,ifelse(breeder==1&success==0,2,3))),
         age_cat=factor(ifelse(age<5,1,ifelse(age>4&age<12,2,3))),
         arr_doy = as.numeric(format(arrived,"%j")),
         breed_next_cat = factor(ifelse(breeder_next == 0,0,ifelse(breeder_next==1&success_next==0,2,3))))

# molt_locs <-locs500%>%
#   mutate(doy=as.numeric(format(time1,"%j")))%>%
#   left_join(dplyr::select(gdr_molt,season,bird_id,bird_fn, start_molt,end_molt,mstart_doy,mend_doy,br_col))%>%
#   filter(doy>=mstart_doy&doy<=mend_doy,inserted==FALSE)%>%
#   mutate(year=factor(season+1))

molt_locs <- read_csv("Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/molt_locs500.csv")

molt_locs_sf <- st_as_sf(molt_locs,coords = c("lon","lat"),
                         crs = "+proj=longlat +ellps=WGS84 +lon_0=-169 +lon_wrap")
molt_locs_df <- df_spatial(st_transform(molt_locs_sf,proj_ant))


data=molt_locs_df%>%
  mutate(month=month(time1))


colony="CROZ"
seasons=2016
months=3
grid_size=50

setwd("Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data")

# Function to summarize molt locations and make figures of summarized data ####
source("code/molt_locs_summary_function.R")


croz_17<-  plot_mlocs(data=data,colony="CROZ",seasons=2016,sex=c("M","F"),grid_size=25000,legend.position="none",xlab="",ylab="Latitude",legend.title = "", title=2017,
                path = "Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/summary_rasters/c_molt_locs_2017.tif")
print(croz_17)

croz_18 <- plot_mlocs(data=data,colony="CROZ",2017,grid_size=25000,legend.title = "",xlab="Latitude",ylab="",legend.position="none",title=2018, 
                      path = "Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/summary_rasters/c_molt_locs_2018.tif")
print(croz_18)

croz_19 <- plot_mlocs(data=data,colony="CROZ",2018,grid_size=25000,legend.title = "",xlab="", ylab="",legend.position="none",title=2019, 
                      path = "Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/summary_rasters/c_molt_locs_2019.tif")
grid.arrange(croz_17,croz_18,croz_19,ncol=3,nrow=1)



royd_17<-  plot_mlocs(data=data,colony="ROYD",seasons=2016,sex=c("M","F"),grid_size=25000,legend.position="none",xlab="",ylab="Latitude",legend.title = "", title=2017,
                      path = "Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/summary_rasters/r_molt_locs_2017.tif")
print(royd_17)

royd_18 <- plot_mlocs(data=data,colony="ROYD",2017,grid_size=25000,legend.title = "",xlab="Latitude",ylab="",legend.position="none",title=2018, 
                      path = "Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/summary_rasters/r_molt_locs_2018.tif")
print(royd_18)

royd_19 <- plot_mlocs(data=data,colony="ROYD",2018,grid_size=25000,legend.title = "",xlab="", ylab="",legend.position="none",title=2019, 
                      path = "Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/summary_rasters/r_molt_locs_2019.tif")
grid.arrange(royd_17,royd_18,royd_19,ncol=3,nrow=1)

cr_all <- plot_mlocs(data=data,colony=c("CROZ","ROYD"),seasons=c(2016:2018),grid_size=25000,legend.title = "",xlab="Longitude", ylab="Latitude",legend.position="none",title="2017-2019", 
                     rast_path = "Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/summary_rasters/cr_molt_locs_all.tif",
                     poly_path = "Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/contours/CROZ_ROYD_all_molt_locs")
print(cr_all)


croz_all <- plot_mlocs(data=data,colony=c("CROZ"),seasons=c(2016:2018),grid_size=50000,legend.title = "",xlab="Longitude", ylab="Latitude",legend.position="none",title="Crozier 2017-2019", 
                       rast_path = "data/summary_rasters/croz_molt_locs_all.tif",
                       poly_path = "data/contours/CROZ_all_molt_locs")

royds_all <- plot_mlocs(data=data,colony=c("ROYD"),seasons=c(2016:2018),grid_size=50000,legend.title = "",xlab="Longitude", ylab="Latitude",legend.position="none",title="Royds 2017-2019", 
                       rast_path = "data/summary_rasters/royd_molt_locs_all.tif",
                       poly_path = "data/contours/ROYD_all_molt_locs")

grid.arrange(royds_all,croz_all,ncol=2,nrow=1)


print(croz_all)


# filter out data near autumn equinox ####
# filter dates to prior to day 86 (based on GB assessment in penguinscience slide deck)
data <- molt_locs_df%>%
  filter(doy<86)%>%
  mutate(month=month(time1))

hist(data$doy)


croz_all <- plot_mlocs(data=data,colony=c("CROZ"),seasons=c(2016:2018),grid_size=50000,legend.title = "",xlab="Longitude", ylab="Latitude",legend.position="none",title="Crozier 2017-2019 not filtered", 
                       rast_path = "data/summary_rasters/croz_molt_locs_all_filt.tif",
                       poly_path = "data/contours/CROZ_all_molt_locs_filt")

royds_all <- plot_mlocs(data=data,colony=c("ROYD"),seasons=c(2016:2018),grid_size=50000,legend.title = "",xlab="Longitude", ylab="Latitude",legend.position="none",title="Royds 2017-2019 not filtered", 
                        rast_path = "data/summary_rasters/royd_molt_locs_all_filt.tif",
                        poly_path = "data/contours/ROYD_all_molt_locs_filt")
grid.arrange(royds_all,croz_all,ncol=2,nrow=1)











# calculate the number of birds in each samp
molt_tab%>%
  filter(br_col=="CROZ",season==2016)%>%
  group_by(bird_id)%>%
  tally()
c17_mlocs_summ<-molt_locs%>%
  filter(br_col=="CROZ",season==2016)%>%
  group_by(bird_id)%>%
  summarise(n_locs=n()/(500))

molt_tab%>%
  filter(br_col=="CROZ",season==2017)%>%
  group_by(bird_id)%>%
  tally()

c18_mlocs_summ<-molt_locs%>%
  filter(br_col=="CROZ",season==2017)%>%
  group_by(bird_id)%>%
  summarise(n_locs=n()/(500))

molt_tab%>%
  filter(br_col=="CROZ",season==2018)%>%
  group_by(bird_id)%>%
  tally()

c19_mlocs_summ<-molt_locs%>%
  filter(br_col=="CROZ",season==2018)%>%
  group_by(bird_id)%>%
  summarise(n_locs=n()/(500))





# calculate the number of birds in each samp
molt_tab%>%
  filter(br_col=="ROYD",season==2016)%>%
  group_by(bird_id)%>%
  tally()

r17_mlocs_summ<-molt_locs%>%
  filter(br_col=="ROYD",season==2016)%>%
  group_by(bird_id)%>%
  summarise(n_locs=n()/(500))

molt_tab%>%
  filter(br_col=="ROYD",season==2017)%>%
  group_by(bird_id)%>%
  tally()

r18_mlocs_summ<-molt_locs%>%
  filter(br_col=="ROYD",season==2017)%>%
  group_by(bird_id)%>%
  summarise(n_locs=n()/(500))

molt_tab%>%
  filter(br_col=="ROYD",season==2018)%>%
  group_by(bird_id)%>%
  tally()

r19_mlocs_summ<-molt_locs%>%
  filter(br_col=="ROYD",season==2018)%>%
  group_by(bird_id)%>%
  summarise(n_locs=n()/(500))
