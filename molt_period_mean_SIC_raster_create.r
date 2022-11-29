#creates mean sea ice (AMSR2 and/or SSMI) rasters for assessment of conditions and change
#utlizing molting time periods and areas determined from the 2017-2019 GDR study
#i.e., what are the ice conditions across the whole area that penguins used 2017-2019 compared
#with where they were during molt?

#Grant
#original version: 10/11/2021
#last update: 11/23/2021
# AS update: 3/3/2022

#Clean up memory as needed
#gc()
## Specify lib paths to the Antarctica Project R packages library
#.libPaths('C:/R/libs') 

##Specify the directory where analyses are stored for this (depends on what network you are on)
analyses_dir<-"Z:/Informatics/S031/analyses/gdr_molt_SIC/"
# analyses_dir<-"Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/"

##Specify the GIS directory you are working from (depends on what network you are on)
GIS_dir<-"V:/Project/Terrestrial/adpe/"
# GIS_dir<-"C:/adpe/"

##Load  any needed libraries####
library(raster)
library(adehabitatHR)
library(rgdal)
library(sp)
library(tidyverse)
library(pastecs) #for stat.desc
library(data.table)
#library(rgeos)##for gDifference


#------------------------------------------------------------------------#
# Calculate dates to include in mean SIC calculation

# read in table with molt locations
molt_locs <- fread("data/molt_locs500_2022-02-15.csv")
# filter to days before autumnal equinox
molt_locs_filt <- molt_locs%>%
  filter(doy<86)
# calculate date ranges for each year
molt_dates <- molt_locs_filt%>%
  group_by(year)%>%
  summarise(min_doy = min(doy),max_doy=max(doy))
# 2017
# min = 52/ Feb 21, max = 85/ March 26
# 2018
# min = 44/ Feb 13, max = 82/ March 23
# 2019
# min = 50/ Feb 19, max = 85/ March 26


# ---------------------------------------------------------------------------------#
# Load home range and combined polygons that will used for clipping SIC raster ####
# setwd("Z:/Informatics/S031/analyses/gdr_molt_SIC")

# define common projection
sic_crs <- CRS("+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") 
molt_locs_dir<-paste0(analyses_dir,"data/contours")

# load molt contours that include all 3 years or 3 yr + previous study
total_molt_poly<-spTransform(readOGR(paste0(molt_locs_dir,"/CROZ_ROYD_molt_locs_filt_95_poly.shp")),sic_crs) # 95% contour for all molt locations together

east_molt50_poly<-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_allyr_filt_50_poly.shp")),sic_crs) #50% contour for eastern molt locations in all years
west_molt50_poly<-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_allyr_filt_50_poly.shp")),sic_crs) #50% contour for western molt locations in all years

east_molt95_poly<-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_allyr_filt_95_poly.shp")),sic_crs)
west_molt95_poly<-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_allyr_filt_95_poly.shp")),sic_crs)

full_hr_poly<-spTransform(readOGR(paste0(molt_locs_dir,"/WinterPolygon2003-05_2017-19.shp")),sic_crs)

#the full_hr_file combines 2003-2005 results with 2017-2019 results for home range 
#note that there are many ways to create these polygons - here we are using the 95% contour
# created with create_molt_contours.R or calculate_indiv_and_EW_molt_hr.R 
# with filter applied to exclude DOY<86 to exclude the positions with the 
#lowest confidence due to equinox.

# load molt contours by year and re-project to SIC projection
east50_2017 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_2017_filt_50_poly.shp")),sic_crs)
east50_2018 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_2018_filt_50_poly.shp")),sic_crs)
east50_2019 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_2019_filt_50_poly.shp")),sic_crs)

east95_2017 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_2017_filt_95_poly.shp")),sic_crs)
east95_2018 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_2018_filt_95_poly.shp")),sic_crs)
east95_2019 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/east_molt_locs_2019_filt_95_poly.shp")),sic_crs)

west50_2017 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_2017_filt_50_poly.shp")),sic_crs)
west50_2018 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_2018_filt_50_poly.shp")),sic_crs)
west50_2019 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_2019_filt_50_poly.shp")),sic_crs)

west95_2017 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_2017_filt_95_poly.shp")),sic_crs)
west95_2018 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_2018_filt_95_poly.shp")),sic_crs)
west95_2019 <-spTransform(readOGR(paste0(molt_locs_dir,"/east_west/west_molt_locs_2019_filt_95_poly.shp")),sic_crs)




#-----------------------------------------------------------------------#
##Calculate sea ice concentration (SIC) for 2017-2019 using SSMI data####
#-----------------------------------------------------------------------#

##Make data frame for holding results
ma_results_ssmi_df<-
  data.frame(year=integer(), full_hr_molt_sic=double(), east_molt50_sic=double(),east_molt95_sic=double(), west_molt50_sic=double(), west_molt95_sic=double(), n_files=double(), stringsAsFactors=F)

# list of east contours
e_cont50 <-list(east50_2017,east50_2018,east50_2019)
names(e_cont50) <- c(2017:2019)

e_cont95 <-list(east95_2017,east95_2018,east95_2019)
names(e_cont95) <- c(2017:2019)

# list of west contours
w_cont50 <- list(west50_2017,west50_2018,west50_2019)
names(w_cont50) <- c(2017:2019)

w_cont95 <- list(west95_2017,west95_2018,west95_2019)
names(w_cont95) <- c(2017:2019)

# loop to run through and calculate mean sic in each contour each year
# this loop is calculating mean for 50 and 95% annual east west contours as well as the 95% area of all 3 years combined
for(yy in c(2017:2019)) {
  myyear=as.character(yy)
  #name the location where SSMI data live
  ssmi_dir=paste0(GIS_dir,"sat_images/sea_ice/geotiff/",myyear,"/")#if on pointblue network
  # ssmi_dir=paste0(GIS_dir,"sat_images/ssmi/geotiff/",myyear,"/")
  setwd(ssmi_dir)
  
  # dates for study years
  # 2017
  # min = 52/ Feb 21, max = 85/ March 26
  # 2018
  # min = 44/ Feb 13, max = 82/ March 23
  # 2019
  # min = 50/ Feb 19, max = 85/ March 26
  
  if(yy==2017) {
    mind<-21 ##Minimum day: DOY52 February 21 - dates calculated above; note no leap days in these 3 years
    maxd<-26 ##Max day: DOY 85; Mar 26
  }
  if(yy==2018) {
    mind<-13 ##DOY44 Feb 13
    maxd<-23 ##DOY82; March 23
  }
  if(yy==2019) {
    mind<-19##DOY50 Feb 19
    maxd<-26##DOY 85; March; 26
  }
  #process the day ranges above:
  files_feb<-NULL
  files_mar<-NULL
  feb_dir<-paste0(ssmi_dir,"02_Feb/")
  mar_dir<-paste0(ssmi_dir,"03_Mar/")
  
  for(d in mind:28) {
    d_char<-as.character(d)
    if(d<10) {
      d_char<-paste0("0",d_char)
    }
    the_feb_file<-paste0(feb_dir,"S_",myyear,"02",d_char,"_concentration_v3.0.tif")
    if(file.exists(the_feb_file)) {
      files_feb <- c(files_feb,the_feb_file)
    }
  }
  for(d in 1:maxd) {
    d_char<-as.character(d)
    if(d<10) {
      d_char<-paste0("0",d_char)
    }
    the_mar_file<-paste0(mar_dir,"S_",myyear,"03",d_char,"_concentration_v3.0.tif")
    if(file.exists(the_mar_file)) {
      files_mar <- c(files_mar,the_mar_file)
    }
    
  }
  
  
  
  # for(d in mind:28) {
  #   d_char<-as.character(d)
  #   if(d<10) {
  #     d_char<-paste0("0",d_char)
  #   }
  #   the_feb_file<-paste0(feb_dir,"S_",myyear,"02",d_char,"_concentration_v3.0.tif")
  #   if(file.exists(the_feb_file)) {
  #     files_feb <- c(files_feb,the_feb_file)
  #   }
  # }
  # for(d in 1:maxd) {
  #   d_char<-as.character(d)
  #   if(d<10) {
  #     d_char<-paste0("0",d_char)
  #   }
  #   the_mar_file<-paste0(mar_dir,"S_",myyear,"03",d_char,"_concentration_v3.0.tif")
  #   if(file.exists(the_mar_file)) {
  #     files_mar <- c(files_mar,the_mar_file)
  #   }
  # }
  # files <- c(files_feb,files_mar)
  # files_list<-as.list(files)
  # STACK2 <- stack(files_list)
  
  files_list <- as.list(c(files_feb,files_mar))
  STACK1 <- stack(files_list)
  
  #need to deal with no-data>1000 in these rasters:
  STACK1 <- STACK1/10
  STACK1[STACK1 > 100] <- NA
  num_files<-nlayers(STACK1)
  
  #calculate the mean ice values####  
  message(paste("calculating continent wide mean sic between feb", mind,"and mar",maxd, myyear))
  means <- calc(STACK1, fun = mean, na.rm = T)
  
  #make raster with the mean values for the appropriate molt time period
  writeRaster(x = means, filename = paste0(analyses_dir,"data/sic/ssmi_2017-2019/mean_ant_sic_ssmi_",myyear,"_molt_dates.tif"), driver = "GeoTiff", overwrite=T)
  # ice_raster_file<-paste0("mean_",myyear,"_molt_dates.tif")
  
  #clip the ice raster to the molt areas
  #East
  e_molt50 <-e_cont50[[paste(myyear)]]
  e_molt50_crop<-crop(means, e_molt50)
  e_molt50_clip<-mask(e_molt50_crop, e_molt50)
  
  e_molt95 <-e_cont95[[paste(myyear)]]
  e_molt95_crop<-crop(means, e_molt95)
  e_molt95_clip<-mask(e_molt95_crop, e_molt95)
  
  #West
  w_molt50 <-w_cont50[[paste(myyear)]]
  w_molt50_crop<-crop(means, w_molt50)
  w_molt50_clip<-mask(w_molt50_crop, w_molt50)
  
  w_molt95 <-w_cont95[[paste(myyear)]]
  w_molt95_crop<-crop(means, w_molt95)
  w_molt95_clip<-mask(w_molt95_crop, w_molt95)
  
  #Total Home Range
  hr_sic_crop<-crop(means, total_molt_poly)
  hr_sic_clip<-mask(hr_sic_crop, total_molt_poly) #so this will be the SIC for the molt period
  #inside the whole HR across 3 years of data
  
  #extract mean SIC from the clipped rasters
  #East
  e50_vals<-getValues(e_molt50_clip)#get raster values
  e50_molt_sic<-mean(e50_vals, na.rm=T)#remove NAs and compute mean
  
  e95_vals<-getValues(e_molt95_clip)#get raster values
  e95_molt_sic<-mean(e95_vals, na.rm=T)#remove NAs and compute mean
  
  
  #West
  w50_vals<-getValues(w_molt50_clip)
  w50_molt_sic<-mean(w50_vals, na.rm=T)
  
  w95_vals<-getValues(w_molt95_clip)
  w95_molt_sic<-mean(w95_vals, na.rm=T)
  
  #total home range SIC that year
  hr_vals<-getValues(hr_sic_clip)
  hr_sic<-mean(hr_vals, na.rm=T)
  
  #update the results table
  e50_molt_sic<-as.numeric(sprintf(e50_molt_sic, fmt = '%#.2f'))
  e95_molt_sic<-as.numeric(sprintf(e95_molt_sic, fmt = '%#.2f'))
  w50_molt_sic<-as.numeric(sprintf(w50_molt_sic, fmt = '%#.2f'))
  w95_molt_sic<-as.numeric(sprintf(w95_molt_sic, fmt = '%#.2f'))
  
  full_hr_sic<-as.numeric(sprintf(hr_sic, fmt= '%#.2f'))
  ma_results_ssmi_df[nrow(ma_results_ssmi_df)+1,] <- c(myyear,
                                                       full_hr_sic, 
                                                       e50_molt_sic,
                                                       e95_molt_sic,
                                                       w50_molt_sic,
                                                       w95_molt_sic,
                                                       num_files)
  
  # ma_results_ssmi_df[nrow(ma_results_ssmi_df)+1,] <- c(myyear, full_hr_sic, east_molt50_sic, east_molt95_sic, west_molt50_sic,west_molt95_sic, num_files)
  
  #write the mean SIC raster and the clipped rasters
  # writeRaster(means, paste0(analyses_dir,"data/sic/molt_mean_sic_",myyear), format="GTiff", overwrite=T)  
  writeRaster(e_molt50_clip, paste0(analyses_dir,"data/sic/ssmi_2017-2019/east_molt50_mean_sic_ssmi_",myyear), format="GTiff", overwrite=T)  
  writeRaster(w_molt50_clip, paste0(analyses_dir,"data/sic/ssmi_2017-2019/west_molt50_mean_sic_ssmi_",myyear), format="GTiff", overwrite=T)
  writeRaster(e_molt95_clip, paste0(analyses_dir,"data/sic/ssmi_2017-2019/east_molt95_mean_sic_ssmi_",myyear), format="GTiff", overwrite=T)  
  writeRaster(w_molt95_clip, paste0(analyses_dir,"data/sic/ssmi_2017-2019/west_molt95_mean_sic_ssmi",myyear), format="GTiff", overwrite=T)  
  writeRaster(hr_sic_clip, paste0(analyses_dir,"data/sic/ssmi_2017-2019/hr_molt_mean_sic_ssmi_",myyear), format="GTiff", overwrite=T)  
}
#write the results table:
write_csv(ma_results_ssmi_df,paste0(analyses_dir,"data/ew_molt_sic_2017_2019_ssmi.csv"))




#-----------------------------------------------------------------------#
##Calculate sea ice concentration (SIC) for 2017-2019 using AMSR data####
#-----------------------------------------------------------------------#


##Make data frame for holding results
ma_results_df<-
  data.frame(year=integer(),
             east_50ma_sic=double(),west_50ma_sic=double(),
             east_95ma_sic=double(),west_95ma_sic=double(),
             total_hr_sic=double(),
             stringsAsFactors=F)
# list of east contours
e_cont50 <-list(east50_2017,east50_2018,east50_2019)
names(e_cont50) <- c(2017:2019)

e_cont95 <-list(east95_2017,east95_2018,east95_2019)
names(e_cont95) <- c(2017:2019)

# list of west contours
w_cont50 <- list(west50_2017,west50_2018,west50_2019)
names(w_cont50) <- c(2017:2019)

w_cont95 <- list(west95_2017,west95_2018,west95_2019)
names(w_cont95) <- c(2017:2019)

# loop to run through and calculate mean sic in each contour each year
# this loop is calculating mean for 50 and 95% annual east west contours as well as the 95% area of all 3 years combined
for(yy in c(2017:2019)) {
  myyear=as.character(yy)
  amsr_dir=paste0(GIS_dir,"nasa_winter_ecology/ice_concentration/raw/y",myyear,"/")
  # amsr_dir=paste0(GIS_dir,"sat_images/AMSR/y",myyear,"/")

  setwd(amsr_dir)
  # 2017
  # min = 52/ Feb 21, max = 85/ March 26
  # 2018
  # min = 44/ Feb 13, max = 82/ March 23
  # 2019
  # min = 50/ Feb 19, max = 85/ March 26

  if(yy==2017) {
    mind<-21 ##Minimum day: DOY52 February 21 - dates calculated above; note no leap days in these 3 years
    maxd<-26 ##Max day: DOY 85; Mar 26
  }
  if(yy==2018) {
    mind<-13 ##DOY44 Feb 13
    maxd<-23 ##DOY82; March 23
  }
  if(yy==2019) {
    mind<-19##DOY50 Feb 19
    maxd<-26##DOY 85; March; 26
  }
  #process the day ranges above:
  files_feb<-NULL
  files_mar<-NULL
  for(d in mind:28) {
    d_char<-as.character(d)
    if(d<10) {
      d_char<-paste0("0",d_char)
    }
    files_feb <- c(files_feb,paste0("sic_",myyear,"02",d_char,".tif"))
  }
  for(d in 1:maxd) {
    d_char<-as.character(d)
    if(d<10) {
      d_char<-paste0("0",d_char)
    }

    files_mar <- c(files_mar,paste0("sic_",myyear,"03",d_char,".tif"))

  }
  files_list <- as.list(c(files_feb,files_mar))
  STACK1 <- stack(files_list)

  #Deal with no-data=120 in these rasters:
  STACK1[STACK1 > 100] <- NA

  #calculate the mean ice values
  message(paste("calculating continent wide mean sic between feb", mind,"and mar",maxd, myyear))
  means <- calc(STACK1, fun = mean, na.rm = T)

  #make raster with the mean values for the appropriate molt time period
  writeRaster(x = means, filename = paste0(analyses_dir,"data/sic/amsr_2017-2019/mean_ant_sic_amsr_",myyear,"_molt_dates.tif"), driver = "GeoTiff", overwrite=T)
  # ice_raster_file<-paste0("mean_",myyear,"_molt_dates.tif")

   #clip the ice raster to the molt areas
  #East
  e_molt50 <-e_cont50[[paste(myyear)]]
  e_molt50_crop<-crop(means, e_molt50)
  e_molt50_clip<-mask(e_molt50_crop, e_molt50)

  e_molt95 <-e_cont95[[paste(myyear)]]
  e_molt95_crop<-crop(means, e_molt95)
  e_molt95_clip<-mask(e_molt95_crop, e_molt95)

  #West
  w_molt50 <-w_cont50[[paste(myyear)]]
  w_molt50_crop<-crop(means, w_molt50)
  w_molt50_clip<-mask(w_molt50_crop, w_molt50)

  w_molt95 <-w_cont95[[paste(myyear)]]
  w_molt95_crop<-crop(means, w_molt95)
  w_molt95_clip<-mask(w_molt95_crop, w_molt95)

  #Total Home Range
  hr_sic_crop<-crop(means, total_molt_poly)
  hr_sic_clip<-mask(hr_sic_crop, total_molt_poly) #so this will be the SIC for the molt period
  #inside the whole HR across 3 years of data

  #extract mean SIC from the clipped rasters
  #East
  e50_vals<-getValues(e_molt50_clip)#get raster values
  e50_molt_sic<-mean(e50_vals, na.rm=T)#remove NAs and compute mean

  e95_vals<-getValues(e_molt95_clip)#get raster values
  e95_molt_sic<-mean(e95_vals, na.rm=T)#remove NAs and compute mean


  #West
  w50_vals<-getValues(w_molt50_clip)
  w50_molt_sic<-mean(w50_vals, na.rm=T)

  w95_vals<-getValues(w_molt95_clip)
  w95_molt_sic<-mean(w95_vals, na.rm=T)

  #total home range SIC that year
  hr_vals<-getValues(hr_sic_clip)
  hr_sic<-mean(hr_vals, na.rm=T)

  #update the results table
  e50_molt_sic<-as.numeric(sprintf(e50_molt_sic, fmt = '%#.2f'))
  e95_molt_sic<-as.numeric(sprintf(e95_molt_sic, fmt = '%#.2f'))
  w50_molt_sic<-as.numeric(sprintf(w50_molt_sic, fmt = '%#.2f'))
  w95_molt_sic<-as.numeric(sprintf(w95_molt_sic, fmt = '%#.2f'))

  hr_sic<-as.numeric(sprintf(hr_sic, fmt= '%#.2f'))
  ma_results_df[nrow(ma_results_df)+1,] <- c(myyear,
                                             e50_molt_sic,w50_molt_sic,
                                             e95_molt_sic,w95_molt_sic,
                                             hr_sic)

  #write the mean SIC raster and the clipped rasters
  # writeRaster(means, paste0(analyses_dir,"data/sic/molt_mean_sic_",myyear), format="GTiff", overwrite=T)
  writeRaster(e_molt50_clip, paste0(analyses_dir,"data/sic/amsr_2017-2019/east_molt50_mean_sic_amsr_",myyear), format="GTiff", overwrite=T)
  writeRaster(w_molt50_clip, paste0(analyses_dir,"data/sic/amsr_2017-2019/west_molt50_mean_sic_amsr_",myyear), format="GTiff", overwrite=T)
  writeRaster(e_molt95_clip, paste0(analyses_dir,"data/sic/amsr_2017-2019/east_molt95_mean_sic_amsr_",myyear), format="GTiff", overwrite=T)
  writeRaster(w_molt95_clip, paste0(analyses_dir,"data/sic/amsr_2017-2019/west_molt95_mean_sic_amsr",myyear), format="GTiff", overwrite=T)
  writeRaster(hr_sic_clip, paste0(analyses_dir,"data/sic/amsr_2017-2019/hr_molt_mean_sic_amsr_",myyear), format="GTiff", overwrite=T)
}
#write the results table:
write.csv(ma_results_df,paste0(analyses_dir,"data/ew_molt_area_sic_summary.csv"),row.names=F)




#-----------------------------------------------------------------------------#
##Calculate SIC during molt period for various regions 1980-2021 using SSMI####
#-----------------------------------------------------------------------------#
#Note - use this code for any assessment of the full range of years
#1980 - 2021; for example, summary of ice concentration in the whole
#home range (just comment/uncomment below).
#make a new results df#

ma_all_year_results_ssmi_df<-
  data.frame(year=integer(), full_hr_molt_sic=double(), east_molt50_sic=double(),east_molt95_sic=double(), west_molt50_sic=double(), west_molt95_sic=double(), n_files=double(), stringsAsFactors=F)


for(yy in c(1980:2021)) {
  myyear=as.character(yy)
  #name the location where SSMI data live
  ssmi_dir=paste0(GIS_dir,"sat_images/sea_ice/geotiff/",myyear,"/")#if on pointblue network
  # ssmi_dir=paste0(GIS_dir,"sat_images/ssmi/geotiff/",myyear,"/")
  setwd(ssmi_dir)
  
  #specify the molt time period
  mind=13
  maxd=26 ## the full range of possible molt dates Feb 13 to Mar 26
  
  #process the day ranges above:
  files_feb<-NULL
  files_mar<-NULL
  feb_dir<-paste0(ssmi_dir,"02_Feb/")
  mar_dir<-paste0(ssmi_dir,"03_Mar/")
  
  for(d in mind:28) {
    d_char<-as.character(d)
    if(d<10) {
      d_char<-paste0("0",d_char)
    }
    the_feb_file<-paste0(feb_dir,"S_",myyear,"02",d_char,"_concentration_v3.0.tif")
    if(file.exists(the_feb_file)) {
      files_feb <- c(files_feb,the_feb_file)
    }
  }
  for(d in 1:maxd) {
    d_char<-as.character(d)
    if(d<10) {
      d_char<-paste0("0",d_char)
    }
    the_mar_file<-paste0(mar_dir,"S_",myyear,"03",d_char,"_concentration_v3.0.tif")
    if(file.exists(the_mar_file)) {
      files_mar <- c(files_mar,the_mar_file)
    }
  }
  files <- c(files_feb,files_mar)
  files_list<-as.list(files)
  STACK2 <- stack(files_list)
  #need to deal with no-data>1000 in these rasters:
  STACK2 <- STACK2/10
  STACK2[STACK2 > 100] <- NA
    
  means <- calc(STACK2, fun = mean, na.rm = T)
  num_files<-nlayers(STACK2)
  writeRaster(x = means, filename = paste0(analyses_dir,"data/sic/ssmi_1980-2019/mean_ssmi_",myyear,"_molt_dates_full_range.tif"), driver = "GeoTiff", overwrite=T)
  # ice_raster_file<-paste0("mean_",myyear,"_molt_dates_full_range_v",Sys.Date(),".tif")
 
  #molt_locs are in:
  #proj_ant <-CRS("+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=180 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
  
  #clip the ice raster to the molt areas
  east_molt50_sic_crop<-crop(means, east_molt50_poly)
  east_molt50_sic_clip<-mask(east_molt50_sic_crop, east_molt50_poly)
  east_molt95_sic_crop<-crop(means, east_molt95_poly)
  east_molt95_sic_clip<-mask(east_molt95_sic_crop, east_molt95_poly)
  
  west_molt50_sic_crop<-crop(means, west_molt50_poly)
  west_molt50_sic_clip<-mask(west_molt50_sic_crop, west_molt50_poly)
  west_molt95_sic_crop<-crop(means, west_molt95_poly)
  west_molt95_sic_clip<-mask(west_molt95_sic_crop, west_molt95_poly)
  
  #clip the ice raster to the Ross sea region (combined nonbreeding locs with previous study)
  full_hr_sic_crop<-crop(means, full_hr_poly)
  full_hr_sic_clip<-mask(full_hr_sic_crop, full_hr_poly)
 
  
  east_molt50_vals<-getValues(east_molt50_sic_clip)
  east_molt50_sic<-round(mean(east_molt50_vals, na.rm=T),2)
  east_molt95_vals<-getValues(east_molt95_sic_clip)
  east_molt95_sic<-round(mean(east_molt95_vals, na.rm=T),2)
  
  west_molt50_vals<-getValues(west_molt50_sic_clip)
  west_molt50_sic<-round(mean(west_molt50_vals, na.rm=T),2)
  west_molt95_vals<-getValues(west_molt95_sic_clip)
  west_molt95_sic<-round(mean(west_molt95_vals, na.rm=T),2)
  
  
  full_hr_vals<-getValues(full_hr_sic_clip)
  full_hr_sic<-round(mean(full_hr_vals, na.rm=T),2)
  #update the results table

  #ma_all_year_results_ssmi_df[nrow(ma_all_year_results_ssmi_df)+1,] <- c(myyear,hr_molt_sic,hr_fdive_sic,num_files)
  ma_all_year_results_ssmi_df[nrow(ma_all_year_results_ssmi_df)+1,] <- c(myyear, full_hr_sic, east_molt50_sic, east_molt95_sic, west_molt50_sic,west_molt95_sic, num_files)
  
  #write the rasters 
  writeRaster(east_molt50_sic_clip, paste0(analyses_dir,"data/sic/ssmi_1980-2019/east_molt50_sic_ssmi_",myyear), format="GTiff", overwrite=T)
  writeRaster(east_molt95_sic_clip, paste0(analyses_dir,"data/sic/ssmi_1980-2019/east_molt95_sic_ssmi_",myyear), format="GTiff", overwrite=T)
  writeRaster(west_molt50_sic_clip, paste0(analyses_dir,"data/sic/ssmi_1980-2019/west_molt50_sic_ssmi_",myyear), format="GTiff", overwrite=T)
  writeRaster(west_molt95_sic_clip, paste0(analyses_dir,"data/sic/ssmi_1980-2019/west_molt95_sic_ssmi_",myyear), format="GTiff", overwrite=T)  
  writeRaster(full_hr_sic_clip, paste0(analyses_dir,"data/sic/ssmi_1980-2019/full_hr_sic_ssmi_",myyear), format="GTiff", overwrite=T)  
  
}

#write.csv(ma_all_year_results_ssmi_df,paste0(analyses_dir,"data/molt_area_hr_sic_summary_all_2003-2005_ssmi.csv"),row.names=F)
write.csv(ma_all_year_results_ssmi_df,paste0(analyses_dir,"data/sic_summary_ssmi_1980-2021.csv"),row.names=F)

# ma_all_year_results_ssmi_df <- read_csv(paste0(analyses_dir,"data/sic_summary_ssmi_1980-2021.csv"))




#-------------------------------------------------------------------------------------#
##Calculate sea ice concentration for a high and average SIC year using AMSR data####
#-------------------------------------------------------------------------------------#
# high sic year = 2003 for all regions (from sic_trends.R)
# average year differs for each region but 2006 was close to average for the whole region and looks reasonable


# loop to run through and calculate mean sic in each contour each year
# this loop is calculating mean for 50 and 95% annual east west contours as well as the 95% area of all 3 years combined
for(yy in c(2006)) {
  myyear=as.character(yy)
  amsr_dir=paste0(GIS_dir,"nasa_winter_ecology/ice_concentration/raw/y",myyear,"/")
  # amsr_dir=paste0(GIS_dir,"sat_images/AMSR/y",myyear,"/")
  
  setwd(amsr_dir)
 
  #specify the molt time period
  mind=13
  maxd=26 ## the full range of possible molt dates Feb 13 to Mar 26
  #process the day ranges above:
  files_feb<-NULL
  files_mar<-NULL
  for(d in mind:28) {
    d_char<-as.character(d)
    if(d<10) {
      d_char<-paste0("0",d_char)
    }
    files_feb <- c(files_feb,paste0("sic_",myyear,"02",d_char,".tif"))
  }
  for(d in 1:maxd) {
    d_char<-as.character(d)
    if(d<10) {
      d_char<-paste0("0",d_char)
    }
    
    files_mar <- c(files_mar,paste0("sic_",myyear,"03",d_char,".tif"))  
    
  }
  files_list <- as.list(c(files_feb,files_mar))
  STACK1 <- stack(files_list)
  
  #Deal with no-data=120 in these rasters:
  STACK1[STACK1 > 100] <- NA
  
  #calculate the mean ice values  
  message(paste("calculating continent wide mean sic between feb", mind,"and mar",maxd, myyear))
  means <- calc(STACK1, fun = mean, na.rm = T)
  
  #make raster with the mean values for the appropriate molt time period
  writeRaster(x = means, filename = paste0(analyses_dir,"data/sic/amsr_2003_2006/mean_ant_sic_amsr_",myyear,"_molt_dates.tif"), driver = "GeoTiff", overwrite=T)
  # ice_raster_file<-paste0("mean_",myyear,"_molt_dates.tif")
  
}
  



#-------------------------------------------------------------------------------------#
##Calculate sea ice concentration for a high and average SIC year using SSMI data####
#-------------------------------------------------------------------------------------#
# high sic year = 2003 for all regions (from sic_trends.R), average = 2006
# average year differs for each region but 2006 was close to average for the whole region and looks reasonable


# loop to create mean SIC rasters for whole continent for specified years and molt period

for(yy in c(2003,2006)) {
  myyear=as.character(yy)
  myyear=as.character(yy)
  #name the location where SSMI data live
  ssmi_dir=paste0(GIS_dir,"sat_images/sea_ice/geotiff/",myyear,"/")#if on pointblue network
  setwd(ssmi_dir)
  
  #specify the molt time period
  mind=13
  maxd=26 ## the full range of possible molt dates Feb 13 to Mar 26
  
  #process the day ranges above:
  files_feb<-NULL
  files_mar<-NULL
  feb_dir<-paste0(ssmi_dir,"02_Feb/")
  mar_dir<-paste0(ssmi_dir,"03_Mar/")
  
  for(d in mind:28) {
    d_char<-as.character(d)
    if(d<10) {
      d_char<-paste0("0",d_char)
    }
    the_feb_file<-paste0(feb_dir,"S_",myyear,"02",d_char,"_concentration_v3.0.tif")
    if(file.exists(the_feb_file)) {
      files_feb <- c(files_feb,the_feb_file)
    }
  }
  for(d in 1:maxd) {
    d_char<-as.character(d)
    if(d<10) {
      d_char<-paste0("0",d_char)
    }
    the_mar_file<-paste0(mar_dir,"S_",myyear,"03",d_char,"_concentration_v3.0.tif")
    if(file.exists(the_mar_file)) {
      files_mar <- c(files_mar,the_mar_file)
    }
  }
  files <- c(files_feb,files_mar)
  files_list<-as.list(files)
  STACK2 <- stack(files_list)
  #need to deal with no-data>1000 in these rasters:
  STACK2 <- STACK2/10
  STACK2[STACK2 > 100] <- NA
  
  # calculate mean SIC
  means <- calc(STACK2, fun = mean, na.rm = T)

  writeRaster(x = means, filename = paste0(analyses_dir,"data/sic/ssmi_2003_2006/mean_ant_sic_ssmi_",myyear,"_molt_dates.tif"), driver = "GeoTiff", overwrite=T)
}







#----------------#
##Plot Results####
#----------------#


ma_all_year_results_ssmi_df%>%
  tidyr::pivot_longer(cols=full_hr_molt_sic:west_molt95_sic, names_to="contour",values_to="sic")%>%
  mutate(year=as.numeric(year),sic=as.numeric(sic))%>%
ggplot(aes(year,sic,col=contour))+
  geom_point()+
  geom_smooth(se = TRUE, method = lm)



ma_all_year_results_df$total_hr_molt_sic_num<-as.numeric(ma_all_year_results_df$total_hr_molt_sic)
ma_all_year_results_df$total_hr_fdive_sic_num<-as.numeric(ma_all_year_results_df$total_hr_fdive_sic)

ma_all_year_results_df$year_num<-as.numeric(ma_all_year_results_df$year)

#molt area results
ggplot(ma_all_year_results_df, aes(x = year_num, y=total_hr_molt_sic_num)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(2003,2021,by=2)) +
  ylab("Sea Ice Concentration (%)") +
  xlab("Year") +
  theme_classic() +
  theme(axis.title=element_text(size=16),axis.text=element_text(size=14))



dat <- ma_all_year_results_ssmi_df%>%
  mutate_all(as.numeric)

m_fullhr<-lm(data=dat,full_hr_molt_sic~year)
summary(m_fullhr)

m_e50<-lm(data=dat,east_molt50_sic~year)
summary(m_e50)

m_e95<-lm(data=dat,east_molt95_sic~year)
summary(m_e95)

m_w50<-lm(data=dat,west_molt50_sic~year)
summary(m_w50)

m_w95<-lm(data=dat,west_molt95_sic~year)
summary(m_w95)


# #------------------------------------------------#
# #Molt area results - 1980 - 2021 (AMSR + SSMI)####
# #------------------------------------------------#
# ssmi_amsr_df$amsr_hr_molt_sic_num<-as.numeric(ssmi_amsr_df$amsr_hr_molt)
# ssmi_amsr_df$amsr_hr_fdive_sic_num<-as.numeric(ssmi_amsr_df$amsr_hr_fdive)
# ssmi_amsr_df$ssmi_hr_molt_sic_num<-as.numeric(ssmi_amsr_df$ssmi_hr_molt)
# ssmi_amsr_df$ssmi_hr_fdive_sic_num<-as.numeric(ssmi_amsr_df$ssmi_hr_fdive)
# ssmi_amsr_df$year_num<-as.numeric(ssmi_amsr_df$year)
# 
# ssmi_amsr_df$total_hr_molt_sic_num<-ssmi_amsr_df$amsr_hr_molt_sic_num
# ssmi_amsr_df$total_hr_fdive_sic_num<-ssmi_amsr_df$amsr_hr_fdive_sic_num
# 
# ssmi_amsr_df$total_hr_molt_sic_num<-dplyr::coalesce(ssmi_amsr_df$total_hr_molt_sic_num,ssmi_amsr_df$ssmi_hr_molt_sic_num)
# ssmi_amsr_df$total_fdive_molt_sic_num<-dplyr::coalesce(ssmi_amsr_df$total_hr_fdive_sic_num,ssmi_amsr_df$ssmi_hr_fdive_sic_num)
# 
# ggplot(ssmi_amsr_df, aes(x = year_num, y=total_fdive_molt_sic_num)) +
#   geom_point() +
#   #geom_smooth(method="lm") +
#   geom_line(aes(x=year_num,y=rep(mean(total_fdive_molt_sic_num),nrow(ssmi_amsr_df))),linetype=2) +
#   scale_y_continuous(breaks=seq(0,60,by=10)) +
#   scale_x_continuous(breaks=seq(1980,2021,by=5)) +
#   ylab("Sea Ice Concentration (%)") +
#   xlab("Year") +
#   theme_classic() +
#   theme(axis.title=element_text(size=16),axis.text=element_text(size=14))
#   
# trend_model_molt<-lm(data=ssmi_amsr_df,total_fdive_molt_sic_num~year_num)
# summary(trend_model_molt)
# mean(ssmi_amsr_df$total_fdive_molt_sic_num)
# stat.desc(ssmi_amsr_df$total_fdive_molt_sic_num)#uses lib pastecs
# #28.05 SE 1.65
# 
# ssmi_amsr_model<-lm(data=ssmi_amsr_df,amsr_hr_molt_sic_num~ssmi_hr_molt_sic_num)
# summary(ssmi_amsr_model)
# #P<0.00000001
# #adj R2 = 0.99
# #predict AMSR values for all years based on SSMI values:
# ssmi_vals<-data.frame(ssmi_hr_molt_sic_num=ssmi_amsr_df$ssmi_hr_molt_sic_num)
# ssmi_amsr_df$pr_amsr_hr_molt_sic<-predict(ssmi_amsr_model,newdata=ssmi_vals)
# stat.desc(ssmi_amsr_df$pr_amsr_hr_molt_sic)
# #mean 31.3 SE 1.8
# 
# write.csv(ssmi_amsr_df, paste0(analyses_dir,"data/molt_area_hr_sic_ssmi_amsr_1980-2021.csv"),row.names=F)
# #next: predict AMSR from SSMI for years with no AMSR
# 
# #foraging dive area results
# ggplot(ssmi_amsr_df, aes(x = year_num, y=total_hr_molt_sic_num)) +
#   geom_point() +
#   geom_smooth(method="lm") +
#   scale_y_continuous(breaks=seq(0,60,by=10)) +
#   scale_x_continuous(breaks=seq(1980,2019,by=5)) +
#   ylab("Sea Ice Concentration (%)") +
#   xlab("Year") +
#   theme_classic()
# 
# trend_model_fdive<-lm(data=ssmi_amsr_df,total_hr_fdive_sic_num~year_num)
# summary(trend_model_fdive)
# 
# #----------------------------------------------#
# #Molt area results - 1980 - 2021 (SSMI only)####
# #----------------------------------------------#
# ssmi_croz_royds_hr_molt<-ma_all_year_results_ssmi_df
# #ssmi_croz_royds_hr_molt$croz_hr_sic<-ssmi_croz_royds_hr_molt$total_hr_molt_sic_num
# #ssmi_croz_royds_hr_molt$royd_hr_sic<-ssmi_croz_royds_hr_molt$total_hr_fdive_sic_num
# 
# ssmi_croz_royds_hr_molt<-mutate(ssmi_croz_royds_hr_molt,
#   full_hr_sic_num=as.numeric(full_hr_molt_sic),
#   croz_hr_molt_sic_num=as.numeric(croz_hr_molt_sic),
#   royd_hr_molt_sic_num=as.numeric(royd_hr_molt_sic),
#   year_num=as.numeric(year)
# )
# 
# #modify the y's below according to what you want to see:
# #full_hr_sic_num
# #croz_hr_molt_sic_num
# #royd_hr_molt_sic_num
# 
# ggplot(ssmi_croz_royds_hr_molt, aes(x = year_num, y=royd_hr_molt_sic_num)) +
#   geom_point() +
#   geom_smooth(method="lm") +
#   geom_line(aes(x=year_num,y=rep(mean(royd_hr_molt_sic_num),nrow(ssmi_croz_royds_hr_molt))),linetype=2) +
#   scale_y_continuous(breaks=seq(0,60,by=10)) +
#   scale_x_continuous(breaks=seq(1980,2021,by=5)) +
#   ylab("Sea Ice Concentration (%)") +
#   xlab("Year") +
#   theme_classic() +
#   theme(axis.title=element_text(size=16),axis.text=element_text(size=14))
# 
# 
# stat.desc(ssmi_croz_royds_hr_molt$royd_hr_molt_sic_num)#uses lib pastecs
# #mean for full home range (2003-2005 + 2017-2019) is 16.95
# #mean for Crozier is 31.54 
# #mean for Royds is 18.59 
# stat.desc(ssmi_croz_royds_hr_molt$full_hr_sic_num[ssmi_croz_royds_hr_molt$year>=2017 & ssmi_croz_royds_hr_molt$year<=2019])
# #7.23% 
# stat.desc(ssmi_croz_royds_hr_molt$croz_hr_molt_sic_num[ssmi_croz_royds_hr_molt$year>=2017 & ssmi_croz_royds_hr_molt$year<=2019])
# #9.4% for Croz
# stat.desc(ssmi_croz_royds_hr_molt$royd_hr_molt_sic_num[ssmi_croz_royds_hr_molt$year>=2017 & ssmi_croz_royds_hr_molt$year<=2019])
# #6.31% for Royds
# 
# trend_model<-lm(data=ssmi_croz_royds_hr_molt,royd_hr_molt_sic_num~year_num)
# summary(trend_model)
