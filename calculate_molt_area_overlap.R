# Description -------------------------------------------------------------

# Code to calculate the overlap in molt contours between years
# Drafted 03/07/2023
# A. Schmidt



# Setup -------------------------------------------------------------------
library(rgdal) #for readOGR, spTransform
library(rgeos)##for gDifference
library(raster) #for area
library(sf)
library(tidyverse)


# Read in polygon layers
# load molt contours by year and re-project to SIC projection
molt_locs_dir<-paste0("data/contours")

#Define projection
proj_ant <-CRS("+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=180 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +datum=WGS84 +units=m +no_defs")

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


# Calculate overlap in polygons -------------------------------------------

# calculate overlap between East 50 polygons in 2017 and 2018 -------------
plot(east50_2017)
plot(east50_2018, add = TRUE)


de50_1718<-gDifference(east50_2017, east50_2018) #this is the part of 2017 not covered by 2018
de50_1817<-gDifference(east50_2018,east50_2017)  #this is the part of 2018 not covered by 2017
plot(de50_1718, col='red', add=T) 
plot(de50_1817, col='blue', add=T)

# intersection
e50_1718_inter <-
  gIntersection(east50_2017, east50_2018)
plot(e50_1718_inter, col = "green", add = TRUE)

# calculate area of intersection
area_e50_1718_int <-
  raster::area(e50_1718_inter)

#want to calculate the percent overlap between these two polygons
area_e50_2017 <-
  raster::area(east50_2017)
area_e50_2018 <-
  raster::area(east50_2018)

prop_overlap_e50_1718 <- 
  area_e50_1718_int/(area_e50_2017 + area_e50_2018 - area_e50_1718_int)
  

# calculate overlap between East 50 polygons in 2018 and 2019 -----------
plot(east50_2018)
plot(east50_2019, add = TRUE)


de50_1819<-gDifference(east50_2018, east50_2019) #this is the part of 2018 not covered by 2019
de50_1918<-gDifference(east50_2019,east50_2018)  #this is the part of 2019 not covered by 2018
plot(de50_1819, col='red', add=T) 
plot(de50_1918, col='blue', add=T)

# intersection
e50_1819_inter <-
  gIntersection(east50_2018, east50_2019)
plot(e50_1819_inter, col = "green", add = TRUE)

# calculate area of intersection
area_e50_1819_int <-
  raster::area(e50_1819_inter)

#want to calculate the percent overlap between these two polygons
area_e50_2019 <-
  raster::area(east50_2019)

prop_overlap_e50_1819 <- 
  area_e50_1819_int/(area_e50_2018 + area_e50_2019 - area_e50_1819_int)


# calculate overlap between East 50 polygons in 2017 and 2019 -----------
plot(east50_2017)
plot(east50_2019, add = TRUE)


de50_1719<-gDifference(east50_2017, east50_2019) #this is the part of 2017 not covered by 2019
de50_1917<-gDifference(east50_2019,east50_2017)  #this is the part of 2019 not covered by 2017
plot(de50_1719, col='red', add=T) 
plot(de50_1917, col='blue', add=T)

# intersection
e50_1719_inter <-
  gIntersection(east50_2017, east50_2019)
plot(e50_1719_inter, col = "green", add = TRUE)

# calculate area of intersection
area_e50_1719_int <-
  raster::area(e50_1719_inter)

#want to calculate the percent overlap between these two polygons
prop_overlap_e50_1719 <- 
  area_e50_1719_int/(area_e50_2017 + area_e50_2019 - area_e50_1719_int)


# # calculate overlap between West 50 polygons in 2017 and 2018 -----------

plot(west50_2017)
plot(west50_2018, add = TRUE)


dw50_1718<-gDifference(west50_2017, west50_2018) #this is the part of 2017 not covered by 2018
dw50_1817<-gDifference(west50_2018,west50_2017)  #this is the part of 2018 not covered by 2017
plot(dw50_1718, col='red', add=T) 
plot(dw50_1817, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_w50_2017 <-
  raster::area(west50_2017)
area_w50_2018 <-
  raster::area(west50_2018)

# intersection
w50_1718_inter <-
  gIntersection(west50_2017, west50_2018)
plot(w50_1718_inter, col = "green", add = TRUE)

# calculate area of intersection
area_w50_1718_int <-
  raster::area(w50_1718_inter)

prop_overlap_w50_1718 <- 
  area_w50_1718_int/(area_w50_2017 + area_w50_2018 - area_w50_1718_int)


# calculate overlap between west 50 polygons in 2018 and 2019 -----------
plot(west50_2018)
plot(west50_2019, add = TRUE)


dw50_1819<-gDifference(west50_2018, west50_2019) #this is the part of 2018 not covered by 2019
dw50_1918<-gDifference(west50_2019,west50_2018)  #this is the part of 2019 not covered by 2018
plot(dw50_1819, col='red', add=T) 
plot(dw50_1918, col='blue', add=T)

# Area of 2019 polygon
area_w50_2019 <-
  raster::area(west50_2019)

# intersection
w50_1819_inter <-
  gIntersection(west50_2018, west50_2019)
plot(w50_1819_inter, col = "green", add = TRUE)


# calculate area of intersection
area_w50_1819_int <-
  raster::area(w50_1819_inter)

prop_overlap_w50_1819 <- 
  area_w50_1819_int/(area_w50_2018 + area_w50_2019 - area_w50_1819_int)



# Calculate overlap between west 50 polygons in 2017 and 2019 -------------

plot(west50_2017)
plot(west50_2019, add = TRUE)


dw50_1719<-gDifference(west50_2017, west50_2019) #this is the part of 2017 not covered by 2019
dw50_1918<-gDifference(west50_2019,west50_2017)  #this is the part of 2019 not covered by 2017
plot(dw50_1719, col='red', add=T) 
plot(dw50_1918, col='blue', add=T)

# Area of 2019 polygon
area_w50_2019 <-
  raster::area(west50_2019)

# intersection
w50_1719_inter <-
  gIntersection(west50_2017, west50_2019)
plot(w50_1719_inter, col = "green", add = TRUE)


# calculate area of intersection
area_w50_1719_int <-
  raster::area(w50_1719_inter)

prop_overlap_w50_1719 <- 
  area_w50_1719_int/(area_w50_2017 + area_w50_2019 - area_w50_1719_int)



# calculate overlap between east 95 polygons in 2017 and 2018 -----------

plot(east95_2017)
plot(east95_2018, add = TRUE)


de95_1718<-gDifference(east95_2017, east95_2018) #this is the part of 2017 not covered by 2018
de95_1817<-gDifference(east95_2018,east95_2017)  #this is the part of 2018 not covered by 2017
plot(de95_1718, col='red', add=T) 
plot(de95_1817, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_e95_2017 <-
  raster::area(east95_2017)

area_e95_2018 <-
  raster::area(east95_2018)

# intersection
e95_1718_inter <-
  gIntersection(east95_2017, east95_2018)
plot(e95_1718_inter, col = "green", add = TRUE)


# calculate area of intersection
area_e95_1718_int <-
  raster::area(e95_1718_inter)

prop_overlap_e95_1718 <- 
  area_e95_1718_int/(area_e95_2017 + area_e95_2018 - area_e95_1718_int)


# calculate overlap between east 95 polygons in 2018 and 2019 -----------
plot(east95_2018)
plot(east95_2019, add = TRUE)


de95_1819<-gDifference(east95_2018, east95_2019) #this is the part of 2018 not covered by 2019
de95_1918<-gDifference(east95_2019,east95_2018)  #this is the part of 2019 not covered by 2018
plot(de95_1819, col='red', add=T) 
plot(de95_1918, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_e95_2019 <-
  raster::area(east95_2019)

# intersection
e95_1819_inter <-
  gIntersection(east95_2018, east95_2019)
plot(e95_1819_inter, col = "green", add = TRUE)


# calculate area of intersection
area_e95_1819_int <-
  raster::area(e95_1819_inter)

prop_overlap_e95_1819 <- 
  area_e95_1819_int/(area_e95_2018 + area_e95_2019 - area_e95_1819_int)



# calculate overlap between east 95 polygons in 2017 and 2019 -----------
plot(east95_2017)
plot(east95_2019, add = TRUE)


de95_1719<-gDifference(east95_2017, east95_2019) #this is the part of 2017 not covered by 2019
de95_1917<-gDifference(east95_2019,east95_2017)  #this is the part of 2019 not covered by 2017
plot(de95_1719, col='red', add=T) 
plot(de95_1917, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
# intersection
e95_1719_inter <-
  gIntersection(east95_2017, east95_2019)
plot(e95_1719_inter, col = "green", add = TRUE)


# calculate area of intersection
area_e95_1719_int <-
  raster::area(e95_1719_inter)

prop_overlap_e95_1719 <- 
  area_e95_1719_int/(area_e95_2017 + area_e95_2019 - area_e95_1719_int)



# calculate overlap between west 95 polygons in 2017 and 2018 -----------

plot(west95_2017)
plot(west95_2018, add = TRUE)


dw95_1718<-gDifference(west95_2017, west95_2018) #this is the part of 2017 not covered by 2018
dw95_1817<-gDifference(west95_2018,west95_2017)  #this is the part of 2018 not covered by 2017
plot(dw95_1718, col='red', add=T) 
plot(dw95_1817, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_w95_2017 <-
  raster::area(west95_2017)

area_w95_2018 <-
  raster::area(west95_2018)

# intersection
w95_1718_inter <-
  gIntersection(west95_2017, west95_2018)
plot(w95_1718_inter, col = "green", add = TRUE)


# calculate area of intersection
area_w95_1718_int <-
  raster::area(w95_1718_inter)

prop_overlap_w95_1718 <- 
  area_w95_1718_int/(area_w95_2017 + area_w95_2018 - area_w95_1718_int)


# calculate overlap between west 95 polygons in 2018 and 2019 -----------
plot(west95_2018)
plot(west95_2019, add = TRUE)


dw95_1819<-gDifference(west95_2018, west95_2019) #this is the part of 2018 not covered by 2019
dw95_1918<-gDifference(west95_2019,west95_2018)  #this is the part of 2019 not covered by 2018
plot(dw95_1819, col='red', add=T) 
plot(dw95_1918, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_w95_2019 <-
  raster::area(west95_2019)

# intersection
w95_1819_inter <-
  gIntersection(west95_2018, west95_2019)
plot(w95_1819_inter, col = "green", add = TRUE)


# calculate area of intersection
area_w95_1819_int <-
  raster::area(w95_1819_inter)

prop_overlap_w95_1819 <- 
  area_w95_1819_int/(area_w95_2018 + area_w95_2019 - area_w95_1819_int)



# calculate overlap between west 95 polygons in 2017 and 2019 -----------
plot(west95_2017)
plot(west95_2019, add = TRUE)


dw95_1719<-gDifference(west95_2017, west95_2019) #this is the part of 2017 not covered by 2019
dw95_1917<-gDifference(west95_2019,west95_2017)  #this is the part of 2019 not covered by 2017
plot(dw95_1719, col='red', add=T) 
plot(dw95_1917, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
# intersection
w95_1719_inter <-
  gIntersection(west95_2017, west95_2019)
plot(w95_1719_inter, col = "green", add = TRUE)


# calculate area of intersection
area_w95_1719_int <-
  raster::area(w95_1719_inter)

prop_overlap_w95_1719 <- 
  area_w95_1719_int/(area_w95_2017 + area_w95_2019 - area_w95_1719_int)


# Make table of overlap values

prop_overlap <- 
  data.frame(MoltRegion = c("East 50%", "East 95%", "West 50%", "West 95%"),
             Overlap1718 = round(c(prop_overlap_e50_1718, prop_overlap_e95_1718, prop_overlap_w50_1718, prop_overlap_w95_1718), 2),
             Overlap1819 = round(c(prop_overlap_e50_1819, prop_overlap_e95_1819, prop_overlap_w50_1819, prop_overlap_w95_1819), 2),
             Overlap1719 = round(c(prop_overlap_e50_1719, prop_overlap_e95_1719, prop_overlap_w50_1719, prop_overlap_w95_1719), 2))

# write.table
write_csv(prop_overlap, "results/prop_overlap.csv")

