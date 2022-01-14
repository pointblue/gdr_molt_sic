# Calculate fraction of individuals from each colony that molt in wester or eastern sectors

# Check if have required packages installed and install if not
list.of.packages <- c("tidyverse","gridExtra","viridis","MASS","data.table","lubridate","fasttime",
                      "sp","raster","rgdal","sf","ggspatial")
# compare to existing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install missing packages
if(length(new.packages)>0) {install.packages(new.packages)}
# load required packages
lapply(list.of.packages, library, character.only = TRUE)


setwd("Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date")

# read in MAP layers if want to plot##
#
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


# load molt locs data
# read in molt locs from 500 chains
molt_locs <- read_csv("data/molt_locs500.csv") # this table was created with "map_molt_locations.R" These locations are unprojected
molt_locs_sf <- st_as_sf(molt_locs,coords = c("lon","lat"),
                         crs = "+proj=longlat +ellps=WGS84 +lon_0=-169 +lon_wrap")
molt_locs_df <- df_spatial(st_transform(molt_locs_sf,proj_ant))

# source function to summarize and plot data
source("code/molt_locs_summary_function.R")


# seas=2017
for (seas in 2017:2019){
  dat <- molt_locs_df%>%
    mutate(month=month(time1),season=year)%>%
    filter(season==seas)
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
  labs(x="Year", y="Fraction of individuals that\n molted East of 180°")+
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

