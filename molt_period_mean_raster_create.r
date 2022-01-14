#creates mean sea ice (AMSR2 and/or SSMI) rasters for assessment of conditions and change
#utlizing molting time periods and areas determined from the 2017-2019 GDR study
#Grant
#original version: 10/11/2021
#last update: 11/23/2021

#Clean up memory as needed
#gc()
## Specify lib paths to the Antarctica Project R packages library####
#.libPaths('C:/R/libs') 

##Specify the directory where analyses are stored for this (depends on what network you are on)
#analyses_dir<-"Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/"
analyses_dir<-"Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/"

##Specify the GIS directory you are working from (depends on what network you are on)
#GIS_dir<-"V:/Project/Terrestrial/adpe/"
GIS_dir<-"C:/adpe/"

##Load  any needed libraries####
library(raster)
library(adehabitatHR)
library(rgdal)
library(sp)
library(ggplot2) #for plotting results
library(pastecs) #for stat.desc
library(dplyr)
#library(rgeos)##for gDifference

#-----------------------------------------------------------------------#
##Calculate sea ice concentration (SIC) for 2017-2019 using AMSR data####
#-----------------------------------------------------------------------#
#Note that this one uses the foraging dive locations for the larger region
#comparison (vs. all locations

##Make data frame for holding results####
ma_results_df<-
  data.frame(year=integer(),croz_cma_sic=double(),royd_cma_sic=double(), 
    total_hr_sic=double(),
    stringsAsFactors=F)

for(yy in c(2017:2019)) {
  myyear=as.character(yy)
  #amsr_dir=paste0(GIS_dir,"nasa_winter_ecology/ice_concentration/raw/y",myyear,"/")
  amsr_dir=paste0(GIS_dir,"sat_images/AMSR/y",myyear,"/")
  
  setwd(amsr_dir)

  if(yy==2017) {
    mind<-20 ##Minimum day: DOY51 February 20 - dates calculated by Annie; note no leap days in these 3 years
    maxd<-31 ##Max day: DOY 90; Mar 31
  }
  if(yy==2018) {
    mind<-4 ##DOY35 Feb 4
    maxd<-17 ##DOY 76; March 17
  }
  if(yy==2019) {
    mind<-4##DOY35 Feb 4
    maxd<-21##DOY 80; March; 21
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
  files <- c(files_feb,files_mar)
  files_list<-as.list(files)
  #print(files)
  files
  STACK1 <- stack(files_list)
  
  #Deal with no-data=120 in these rasters:####
  STACK1[STACK1 > 100] <- NA

  #calculate the mean ice values####  
  means <- calc(STACK1, fun = mean, na.rm = T)
  
  #make raster with the mean values for the appropriate molt time period####
  writeRaster(x = means, filename = paste0("mean_",myyear,"_molt_dates.tif"), driver = "GeoTiff", overwrite=T)
  ice_raster_file<-paste0("mean_",myyear,"_molt_dates.tif")
  
  #clip the mean raster to location(s) of interest####
  #molt_locs_dir<-"Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/contours"
  molt_locs_dir<-"Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/contours"
  c_molt_ca_file<-paste0(molt_locs_dir,"/CROZ_",myyear,"_molt_locs_50_poly.shp")#Crozier core molt areas for this year (50% probability)
  r_molt_ca_file<-paste0(molt_locs_dir,"/ROYD_",myyear,"_molt_locs_50_poly.shp")#Royds core molt area for this year
  hr_poly_file<-paste0(molt_locs_dir,"/CROZ_ROYDS_all_fdive_locs_95_poly.shp")#Larger area for comparison
  #i.e., what are the ice conditions across the whole area that penguins used for foraging 2017-2019 compared
  #with where they were during molt?
  
  #load the mean SIC raster:
  ice_raster<-raster(ice_raster_file)
  #load the molt core area polygons for Crozier and Royds
  #note that these need to be polygon spatial dataframes, not lines
  c_molt_ca<-readOGR(c_molt_ca_file)#Crozier
  r_molt_ca<-readOGR(r_molt_ca_file)#Royds
  hr_poly<-readOGR(hr_poly_file)#larger area
  #check on projections - need to match ice data to penguin molt locations
  sic_crs<-crs(ice_raster)
  #molt_locs are in:
  #proj_ant <-CRS("+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=180 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
  
  #put the clipping polygons into the SIC projection:
  c_molt_ca<-spTransform(c_molt_ca, sic_crs)
  r_molt_ca<-spTransform(r_molt_ca, sic_crs)
  hr_poly<-spTransform(hr_poly, sic_crs)
  
  #clip the ice raster to the molt areas
  #CROZ
  c_molt_ca_crop<-crop(ice_raster, c_molt_ca)
  c_molt_ca_clip<-mask(c_molt_ca_crop, c_molt_ca)
  
  #ROYD
  r_molt_ca_crop<-crop(ice_raster, r_molt_ca)
  r_molt_ca_clip<-mask(r_molt_ca_crop, r_molt_ca)

  #Total Home Range
  hr_sic_crop<-crop(ice_raster, hr_poly)
  hr_sic_clip<-mask(hr_sic_crop, hr_poly) #so this will be the SIC for the molt period
  #inside the whole HR across 3 years of data
  
  #extract mean SIC from the clipped rasters####
  #CROZ
  c_vals<-getValues(c_molt_ca_clip)#get raster values
  c_molt_sic<-mean(c_vals, na.rm=T)#remove NAs and compute mean
  
  #ROYD
  r_vals<-getValues(r_molt_ca_clip)
  r_molt_sic<-mean(r_vals, na.rm=T)
  
  #total home range SIC that year
  hr_vals<-getValues(hr_sic_clip)
  hr_sic<-mean(hr_vals, na.rm=T)
  
  #update the results table
  c_molt_sic<-as.numeric(sprintf(c_molt_sic, fmt = '%#.2f'))
  r_molt_sic<-as.numeric(sprintf(r_molt_sic, fmt = '%#.2f'))
  hr_sic<-as.numeric(sprintf(hr_sic, fmt= '%#.2f'))
  ma_results_df[nrow(ma_results_df)+1,] <- c(myyear,c_molt_sic,r_molt_sic,hr_sic)
  
  #write the mean SIC raster and the clipped rasters#### 
  writeRaster(ice_raster, paste0(analyses_dir,"data/summary_rasters/molt_sic_raw_",myyear), format="GTiff", overwrite=T)  
  writeRaster(c_molt_ca_clip, paste0(analyses_dir,"data/summary_rasters/croz_molt_sic_raw_",myyear), format="GTiff", overwrite=T)  
  writeRaster(r_molt_ca_clip, paste0(analyses_dir,"data/summary_rasters/royd_molt_sic_raw_",myyear), format="GTiff", overwrite=T)  
  writeRaster(hr_sic_clip, paste0(analyses_dir,"data/summary_rasters/hr_molt_sic_raw_all_yrs"), format="GTiff", overwrite=T)  
  }
#write the results table:
write.csv(ma_results_df,paste0(analyses_dir,"data/molt_area_sic_summary.csv"),row.names=F)


#---------------------------------------------------------------------------------------------------------#
##Calculate SIC for 95% (HR) of molt areas 2017-2021 and 95% foraging area for all AMSR years 2003-2021####
#---------------------------------------------------------------------------------------------------------#
#Note that subsequent to this we have mostly been looking at only SSMI data for consistency of availability
#across the longer time series. Also note difference between using all locations (or 95%) vs. only foraging
#dive locations.

#make a new results df#
ma_all_year_results_df<-
  data.frame(year=integer(), total_hr_molt_sic=double(), total_hr_fdive_sic=double(), n_files=double(), stringsAsFactors=F)

#note that AMSR-E was the sensor through 2011
#AMSR2 online as of May 2012 (but we don't seem to have the 2012 data)

for(yy in c(2003:2011,2013:2021)) {
  ##no 2012 data because AMSR-E failed Oct 2011 and AMSR-2 online May 2012
  myyear=as.character(yy)
  #amsr_dir=paste0(GIS_dir,"nasa_winter_ecology/ice_concentration/raw/y",myyear,"/")
  amsr_dir=paste0(GIS_dir,"sat_images/AMSR/y",myyear,"/")
  setwd(amsr_dir)

  mind=4
  maxd=31 ## the full range of possible molt dates Feb 4 to Mar 31
  
  #process the day ranges above:
  files_feb<-NULL
  files_mar<-NULL
  
  for(d in mind:28) {
    d_char<-as.character(d)
    if(d<10) {
      d_char<-paste0("0",d_char)
    }
    the_feb_file<-paste0("sic_",myyear,"02",d_char,".tif")
    if(file.exists(the_feb_file)) {
    files_feb <- c(files_feb,the_feb_file)
    }
  }
  for(d in 1:maxd) {
    d_char<-as.character(d)
    if(d<10) {
      d_char<-paste0("0",d_char)
    }
    the_mar_file<-paste0("sic_",myyear,"03",d_char,".tif")
    if(file.exists(the_mar_file)) {
      files_mar <- c(files_mar,the_mar_file)
    }
  }
  files <- c(files_feb,files_mar)
  files_list<-as.list(files)
  #print(files)
  #files
  STACK2 <- stack(files_list)
  #Deal with no-data=120 in these rasters:##
  STACK2[STACK2 > 100] <- NA
  
  means <- calc(STACK2, fun = mean, na.rm = T)
  num_files<-nlayers(STACK2)
  
  means_dir<-paste0(GIS_dir,"nasa_winter_ecology/ice_concentration/means/y",myyear,"/")
  writeRaster(x = means, filename = paste0(means_dir,"mean_",myyear,"_molt_dates_full_range.tif"), driver = "GeoTiff", overwrite=T)
  ice_raster_file<-paste0(means_dir,"mean_",myyear,"_molt_dates_full_range.tif")
  molt_locs_dir<-paste0(analyses_dir,"data/contours")
  combined_molt_hr_file<-paste0(molt_locs_dir,"/CROZ_ROYD_all_molt_locs_95_poly.shp")
  combined_fdive_hr_file<-paste0(molt_locs_dir,"/CROZ_ROYDS_all_fdive_locs_95_poly.shp")
  #i.e., what are the ice conditions across the whole area that penguins used for foraging 2017-2019 compared
  #with where they were during molt?
  
  #load the mean SIC raster:
  ice_raster<-raster(ice_raster_file)
  #load the 95% molt and fdive polygons for Crozier and Royds (all years and colonies combined)
  #note that these need to be polygon spatial dataframes, not lines
  hr_molt_poly<-readOGR(combined_molt_hr_file)
  hr_fdive_poly<-readOGR(combined_fdive_hr_file)
  #check on projections - need to match ice data to penguin molt locations
  sic_crs<-crs(ice_raster)
  #molt_locs are in:
  #proj_ant <-CRS("+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=180 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
  
  #get the clipping polygon data into the SIC projection:
  hr_molt_poly<-spTransform(hr_molt_poly, sic_crs)
  hr_fdive_poly<-spTransform(hr_fdive_poly, sic_crs)  
  #clip the ice raster to the molt areas
  #Total Home Range
  hr_molt_sic_crop<-crop(ice_raster, hr_molt_poly)
  hr_molt_sic_clip<-mask(hr_molt_sic_crop, hr_molt_poly)#so this will be the SIC for the molt period
  #inside the whole molt HR across 3 years of data
  
  #clip the ice raster to the foragind dive 95% HR area
  hr_fdive_sic_crop<-crop(ice_raster, hr_fdive_poly)
  hr_fdive_sic_clip<-mask(hr_fdive_sic_crop, hr_fdive_poly) #so this will be the SIC for the molt period
  #inside the whole foraging dive HR across 3 years of data
  
  #extract mean SIC from the clipped rasters
  #total home range SIC that year
  hr_molt_vals<-getValues(hr_molt_sic_clip)
  hr_molt_sic<-mean(hr_molt_vals, na.rm=T)
  hr_fdive_vals<-getValues(hr_fdive_sic_clip)
  hr_fdive_sic<-mean(hr_fdive_vals, na.rm=T)
  
  #update the results table
  hr_molt_sic<-as.numeric(sprintf(hr_molt_sic, fmt= '%#.2f'))
  hr_fdive_sic<-as.numeric(sprintf(hr_fdive_sic, fmt= '%#.2f'))
  
  ma_all_year_results_df[nrow(ma_all_year_results_df)+1,] <- c(myyear,hr_molt_sic, hr_fdive_sic, num_files)
  
  #write the rasters 
  writeRaster(hr_molt_sic_clip, paste0(analyses_dir,"data/summary_rasters/hr_molt_sic_",myyear), format="GTiff", overwrite=T)  
  writeRaster(hr_fdive_sic_clip, paste0(analyses_dir,"data/summary_rasters/hr_fdive_sic_",myyear), format="GTiff", overwrite=T)  
  
}

#write the results table
write.csv(ma_all_year_results_df,paste0(analyses_dir,"data/molt_area_hr_amsr_sic_summary_2003-2021.csv"),row.names=F)

#-----------------------------------------------------------------------------#
##Calculate SIC during molt period for various regions 1980-2021 using SSMI####
#-----------------------------------------------------------------------------#
#Note - use this code for any assessment of the full range of years
#1980 - 2021; for example, summary of ice concentration in the whole
#home range (just comment/uncomment below).
#make a new results df#
#ma_all_year_results_ssmi_df<-
#  data.frame(year=integer(), total_hr_molt_sic=double(), total_hr_fdive_sic=double(), n_files=double(), stringsAsFactors=F)

#ma_all_year_results_ssmi_df<-
#  data.frame(year=integer(), croz_hr_molt_sic=double(), royd_hr_molt_sic=double(), n_files=double(), stringsAsFactors=F)

ma_all_year_results_ssmi_df<-
  data.frame(year=integer(), full_hr_molt_sic=double(), croz_hr_molt_sic=double(), royd_hr_molt_sic=double(), n_files=double(), stringsAsFactors=F)

for(yy in c(1980:2021)) {
  myyear=as.character(yy)
  #name the location where SSMI data live
  #ssmi_dir=paste0("GIS_dir,"sat_images/sea_ice/geotiff/",myyear,"/")#if on pointblue network
  ssmi_dir=paste0(GIS_dir,"sat_images/ssmi/geotiff/",myyear,"/")
  setwd(ssmi_dir)
  
  #specify the molt time period
  mind=4
  maxd=31 ## the full range of possible molt dates Feb 4 to Mar 31
  
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
  writeRaster(x = means, filename = paste0("mean_",myyear,"_molt_dates_full_range.tif"), driver = "GeoTiff", overwrite=T)
  ice_raster_file<-paste0("mean_",myyear,"_molt_dates_full_range.tif")
  molt_locs_dir<-paste0(analyses_dir,"data/contours")
  
  #combined_molt_hr_file<-paste0(molt_locs_dir,"/CROZ_ROYD_all_molt_locs_95_poly.shp")
  croz_molt_hr_file<-paste0(molt_locs_dir,"/CROZ_all_molt_locs_filt_95_poly_arc.shp")
  royd_molt_hr_file<-paste0(molt_locs_dir,"/ROYD_all_molt_locs_filt_95_poly_arc.shp")
  full_hr_file<-"Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/contours/WinterPolygon2003-05_2017-19.shp"
  #the full_hr_file combines 2003-2005 results with 2017-2019 results for home range 
  #note that there are many ways to create these polygons - here we are using the 95% contour
  #created using ArcMap with filter applied to exclude DOY<86 to exclude the positions with the 
  #lowest confidence due to equinox.
  #combined_fdive_hr_file<-paste0(molt_locs_dir,"/CROZ_ROYDS_all_fdive_locs_95_poly.shp")
  #i.e., what are the ice conditions across the whole area that penguins used 2017-2019 compared
  #with where they were during molt?
  
  #load the raster:
  ice_raster<-raster(ice_raster_file)
  #load the 95% molt and fdive polygons for Crozier and Royds (all years and colonies combined)
  #note that these need to be polygon spatial dataframes, not lines
  #hr_molt_poly<-readOGR(combined_molt_hr_file)
  #hr_fdive_poly<-readOGR(combined_fdive_hr_file)
  croz_molt_poly<-readOGR(croz_molt_hr_file)
  royd_molt_poly<-readOGR(royd_molt_hr_file)
  full_hr_poly<-readOGR(full_hr_file)
  #check on projections - need to match ice data to penguin molt locations
  sic_crs<-crs(ice_raster)
  #molt_locs are in:
  #proj_ant <-CRS("+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=180 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
  
  #get thepolygon data into the SIC projection:
  #hr_molt_poly<-spTransform(hr_molt_poly, sic_crs)
  #hr_fdive_poly<-spTransform(hr_fdive_poly, sic_crs)  
  croz_molt_poly<-spTransform(croz_molt_poly, sic_crs)
  royd_molt_poly<-spTransform(royd_molt_poly, sic_crs)
  full_hr_poly<-spTransform(full_hr_poly, sic_crs)
  #clip the ice raster to the molt areas
  #Total Home Range
  #hr_molt_sic_crop<-crop(ice_raster, hr_molt_poly)
  #hr_molt_sic_clip<-mask(hr_molt_sic_crop, hr_molt_poly) #so this will be the SIC for the molt period
  #inside the whole molt HR across 3 years of data
  croz_molt_sic_crop<-crop(ice_raster, croz_molt_poly)
  croz_molt_sic_clip<-mask(croz_molt_sic_crop, croz_molt_poly)
  
  royd_molt_sic_crop<-crop(ice_raster, royd_molt_poly)
  royd_molt_sic_clip<-mask(royd_molt_sic_crop, royd_molt_poly)

  full_hr_sic_crop<-crop(ice_raster, full_hr_poly)
  full_hr_sic_clip<-mask(full_hr_sic_crop, full_hr_poly)
  #clip the ice raster to the foraging dive 95% HR area
  #hr_fdive_sic_crop<-crop(ice_raster, hr_fdive_poly)
  #hr_fdive_sic_clip<-mask(hr_fdive_sic_crop, hr_fdive_poly) #so this will be the SIC for the molt period
  #inside the whole foraging dive HR across 3 years of data
  
  #extract mean SIC from the clipped rasters
  #total home range SIC that year
  #hr_molt_vals<-getValues(hr_molt_sic_clip)
  #hr_molt_sic<-mean(hr_molt_vals, na.rm=T)
  #hr_fdive_vals<-getValues(hr_fdive_sic_clip)
  #hr_fdive_sic<-mean(hr_fdive_vals, na.rm=T)
  
  croz_molt_vals<-getValues(croz_molt_sic_clip)
  croz_molt_sic<-mean(croz_molt_vals, na.rm=T)
  royd_molt_vals<-getValues(royd_molt_sic_clip)
  royd_molt_sic<-mean(royd_molt_vals, na.rm=T)
  full_hr_vals<-getValues(full_hr_sic_clip)
  full_hr_sic<-mean(full_hr_vals, na.rm=T)
  #update the results table
  #hr_molt_sic<-as.numeric(sprintf(hr_molt_sic, fmt= '%#.2f'))
  #hr_fdive_sic<-as.numeric(sprintf(hr_fdive_sic, fmt= '%#.2f'))
  croz_molt_sic<-as.numeric(sprintf(croz_molt_sic, fmt= '%#.2f'))
  royd_molt_sic<-as.numeric(sprintf(royd_molt_sic, fmt= '%#.2f'))
  full_hr_sic<-as.numeric(sprintf(full_hr_sic, fmt= '%#.2f'))
  
  #ma_all_year_results_ssmi_df[nrow(ma_all_year_results_ssmi_df)+1,] <- c(myyear,hr_molt_sic,hr_fdive_sic,num_files)
  ma_all_year_results_ssmi_df[nrow(ma_all_year_results_ssmi_df)+1,] <- c(myyear, full_hr_sic, croz_molt_sic, royd_molt_sic, num_files)
  
  #write the rasters 
  #writeRaster(hr_molt_sic_clip, paste0(analyses_dir,"data/summary_rasters/hr_molt_sic_",myyear), format="GTiff", overwrite=T)  
  #writeRaster(hr_fdive_sic_clip, paste0(analyses_dir,"data/summary_rasters/hr_fdive_sic_",myyear), format="GTiff", overwrite=T) 
  writeRaster(croz_molt_sic_clip, paste0(analyses_dir,"data/summary_rasters/croz_molt_sic_",myyear), format="GTiff", overwrite=T)
  writeRaster(royd_molt_sic_clip, paste0(analyses_dir,"data/summary_rasters/royd_molt_sic_",myyear), format="GTiff", overwrite=T)  
  writeRaster(full_hr_sic_clip, paste0(analyses_dir,"data/summary_rasters/full_hr_sic_",myyear), format="GTiff", overwrite=T)  
  
}

#write.csv(ma_all_year_results_ssmi_df,paste0(analyses_dir,"data/molt_area_hr_sic_summary_all_2003-2005_ssmi.csv"),row.names=F)
write.csv(ma_all_year_results_ssmi_df,paste0(analyses_dir,"data/hr_sic_summary_1980-2021.csv"),row.names=F)

#----------------#
##Plot Results####
#----------------#
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


trend_model_molt<-lm(data=ma_all_year_results_df,total_hr_molt_sic_num~year_num)
summary(trend_model_molt)

#foraging dive ("total HR") area results
ggplot(ma_all_year_results_df, aes(x = year_num, y=total_hr_molt_sic_num)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(2003,2019,by=2)) +
  ylab("Sea Ice Concentration (%)") +
  xlab("Year") +
  theme_classic()

trend_model_fidve<-lm(data=ma_all_year_results_df,total_hr_fdive_sic_num~year_num)
summary(trend_model)

#------------------------------------------------#
#Molt area results - 1980 - 2021 (AMSR + SSMI)####
#------------------------------------------------#
ssmi_amsr_df$amsr_hr_molt_sic_num<-as.numeric(ssmi_amsr_df$amsr_hr_molt)
ssmi_amsr_df$amsr_hr_fdive_sic_num<-as.numeric(ssmi_amsr_df$amsr_hr_fdive)
ssmi_amsr_df$ssmi_hr_molt_sic_num<-as.numeric(ssmi_amsr_df$ssmi_hr_molt)
ssmi_amsr_df$ssmi_hr_fdive_sic_num<-as.numeric(ssmi_amsr_df$ssmi_hr_fdive)
ssmi_amsr_df$year_num<-as.numeric(ssmi_amsr_df$year)

ssmi_amsr_df$total_hr_molt_sic_num<-ssmi_amsr_df$amsr_hr_molt_sic_num
ssmi_amsr_df$total_hr_fdive_sic_num<-ssmi_amsr_df$amsr_hr_fdive_sic_num

ssmi_amsr_df$total_hr_molt_sic_num<-dplyr::coalesce(ssmi_amsr_df$total_hr_molt_sic_num,ssmi_amsr_df$ssmi_hr_molt_sic_num)
ssmi_amsr_df$total_fdive_molt_sic_num<-dplyr::coalesce(ssmi_amsr_df$total_hr_fdive_sic_num,ssmi_amsr_df$ssmi_hr_fdive_sic_num)

ggplot(ssmi_amsr_df, aes(x = year_num, y=total_fdive_molt_sic_num)) +
  geom_point() +
  #geom_smooth(method="lm") +
  geom_line(aes(x=year_num,y=rep(mean(total_fdive_molt_sic_num),nrow(ssmi_amsr_df))),linetype=2) +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(1980,2021,by=5)) +
  ylab("Sea Ice Concentration (%)") +
  xlab("Year") +
  theme_classic() +
  theme(axis.title=element_text(size=16),axis.text=element_text(size=14))
  
trend_model_molt<-lm(data=ssmi_amsr_df,total_fdive_molt_sic_num~year_num)
summary(trend_model_molt)
mean(ssmi_amsr_df$total_fdive_molt_sic_num)
stat.desc(ssmi_amsr_df$total_fdive_molt_sic_num)#uses lib pastecs
#28.05 SE 1.65

ssmi_amsr_model<-lm(data=ssmi_amsr_df,amsr_hr_molt_sic_num~ssmi_hr_molt_sic_num)
summary(ssmi_amsr_model)
#P<0.00000001
#adj R2 = 0.99
#predict AMSR values for all years based on SSMI values:
ssmi_vals<-data.frame(ssmi_hr_molt_sic_num=ssmi_amsr_df$ssmi_hr_molt_sic_num)
ssmi_amsr_df$pr_amsr_hr_molt_sic<-predict(ssmi_amsr_model,newdata=ssmi_vals)
stat.desc(ssmi_amsr_df$pr_amsr_hr_molt_sic)
#mean 31.3 SE 1.8

write.csv(ssmi_amsr_df, paste0(analyses_dir,"data/molt_area_hr_sic_ssmi_amsr_1980-2021.csv"),row.names=F)
#next: predict AMSR from SSMI for years with no AMSR

#foraging dive area results
ggplot(ssmi_amsr_df, aes(x = year_num, y=total_hr_molt_sic_num)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(1980,2019,by=5)) +
  ylab("Sea Ice Concentration (%)") +
  xlab("Year") +
  theme_classic()

trend_model_fdive<-lm(data=ssmi_amsr_df,total_hr_fdive_sic_num~year_num)
summary(trend_model_fdive)

#----------------------------------------------#
#Molt area results - 1980 - 2021 (SSMI only)####
#----------------------------------------------#
ssmi_croz_royds_hr_molt<-ma_all_year_results_ssmi_df
#ssmi_croz_royds_hr_molt$croz_hr_sic<-ssmi_croz_royds_hr_molt$total_hr_molt_sic_num
#ssmi_croz_royds_hr_molt$royd_hr_sic<-ssmi_croz_royds_hr_molt$total_hr_fdive_sic_num

ssmi_croz_royds_hr_molt<-mutate(ssmi_croz_royds_hr_molt,
  full_hr_sic_num=as.numeric(full_hr_molt_sic),
  croz_hr_molt_sic_num=as.numeric(croz_hr_molt_sic),
  royd_hr_molt_sic_num=as.numeric(royd_hr_molt_sic),
  year_num=as.numeric(year)
)

#modify the y's below according to what you want to see:
#full_hr_sic_num
#croz_hr_molt_sic_num
#royd_hr_molt_sic_num

ggplot(ssmi_croz_royds_hr_molt, aes(x = year_num, y=royd_hr_molt_sic_num)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_line(aes(x=year_num,y=rep(mean(royd_hr_molt_sic_num),nrow(ssmi_croz_royds_hr_molt))),linetype=2) +
  scale_y_continuous(breaks=seq(0,60,by=10)) +
  scale_x_continuous(breaks=seq(1980,2021,by=5)) +
  ylab("Sea Ice Concentration (%)") +
  xlab("Year") +
  theme_classic() +
  theme(axis.title=element_text(size=16),axis.text=element_text(size=14))


stat.desc(ssmi_croz_royds_hr_molt$royd_hr_molt_sic_num)#uses lib pastecs
#mean for full home range (2003-2005 + 2017-2019) is 16.95
#mean for Crozier is 31.54 
#mean for Royds is 18.59 
stat.desc(ssmi_croz_royds_hr_molt$full_hr_sic_num[ssmi_croz_royds_hr_molt$year>=2017 & ssmi_croz_royds_hr_molt$year<=2019])
#7.23% 
stat.desc(ssmi_croz_royds_hr_molt$croz_hr_molt_sic_num[ssmi_croz_royds_hr_molt$year>=2017 & ssmi_croz_royds_hr_molt$year<=2019])
#9.4% for Croz
stat.desc(ssmi_croz_royds_hr_molt$royd_hr_molt_sic_num[ssmi_croz_royds_hr_molt$year>=2017 & ssmi_croz_royds_hr_molt$year<=2019])
#6.31% for Royds

trend_model<-lm(data=ssmi_croz_royds_hr_molt,royd_hr_molt_sic_num~year_num)
summary(trend_model)
