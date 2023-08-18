#Script to download and process AMSR2 sea ice concentration data

library(raster) #for reading raster
library(ncdf4) #for accessing hdf file
library(rgdal) #for reading shapefiles
library(lubridate) #for splitting date components
library(tictoc)
###
#set directories
#baseDir<-"V:/Project/Terrestrial/adpe/nasa_winter_ecology/ice_concentration/"
#rawDir<-"V:/Project/Terrestrial/adpe/nasa_winter_ecology/ice_concentration/raw/"
#processedDir<-"V:/Project/Terrestrial/adpe/nasa_winter_ecology/ice_concentration/processed/"

baseDir<-r"(V:\Project\Terrestrial\adpe\nasa_winter_ecology\ice_concentration\)"
rawDir<-r"(V:\Project\Terrestrial\adpe\nasa_winter_ecology\ice_concentration\raw\)"
processedDir<-r"(V:\Project\Terrestrial\adpe\nasa_winter_ecology\ice_concentration\processed\)"


baseURL<-"https://seaice.uni-bremen.de/data/amsr2/asi_daygrid_swath/s6250/"  #2019/jan/Antarctic/asi-AMSR2-s6250-20190123-v5.4.tif
baseFileName<-"asi-AMSR2-s6250-"
shapeDir<-"Z:/Informatics/S031/analyses/SeaIce/masks" #no final backslash
# shapeDir<-"C:/gballard/S031/analyses/SeaIce/masks" #no final backslash
shapeName<-"WinterPolygon2003-2005"

#This is the geodetically preferred alternative to NSIDC polar sterographic projection with WGS84
pss84<-"+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#load wintering area shapefile
winter<-readOGR(dsn=shapeDir,layer=shapeName)
winter<-spTransform(winter,pss84)


#function to get all dates in a given year
#Set the date range across the year here i.e. the wintering period we are interested in
getDays <- function(year){
     as.data.frame(seq(as.Date(paste(year, "-05-17", sep="")), as.Date(paste(year, "-12-31", sep="")), by="+1 day"))
} 
	 

dd<-getDays(2022)
names(dd) <- "date"


tic("whole loop")
for(ii in 1:nrow(dd)){


yr<-year(as.Date(dd[ii,]))
mnth<-month(as.Date(dd[ii,]))
day<-day(as.Date(dd[ii,]))
mnthname<-tolower(month(as.Date(dd[ii,]),label=TRUE,abbr=TRUE))
yrday<-yday(as.Date(dd[ii,]))

mnth<-ifelse(mnth<10, paste0("0", mnth), mnth)
day<-ifelse(day<10, paste0("0", day), day)
#doubleday<-ifelse(yrday<10, paste0("00", yrday), ifelse(yrday>10 & yrday<100, paste0("0",yrday),yrday))

urlDir<-paste0(baseURL,yr,"/",mnthname,"/Antarctic/",baseFileName,yr,mnth,day,"-v5.4.tif")
out<-paste0(rawDir,"y",yr,"/sic_",yr,mnth,day,".tif",sep="")
finalFile<-paste0(processedDir,"y",yr,"/sic_",yr,mnth,day,".tif",sep="")

print(urlDir)
print(out)
tic('downloading')
    w<-1

    options(timeout=20)

    while(inherits(w,"try-error") | w != 0){

     w<-try(download.file(urlDir, out, mode="wb",quiet = FALSE),silent=TRUE)
    }
	toc()
	
	
tic("crop, write")	
#loadlayer and project to WGS_1984_NSIDC_Sea_Ice_Polar_Stereographic_South
mysic <- raster(out)

#SIC already on NSIDC pss wgs84 no need to project to this
#crop to study extent
mysiccrop<-crop(mysic,winter)
mysiccrop<-mask(mysiccrop,winter)

#Need to resample here to match 1km grids?


writeRaster(mysiccrop,filename=finalFile,format='GTiff',overwrite=T)

toc()



}
toc()

