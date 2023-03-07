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
e50_1718_intersection <-
  gIntersection(east50_2017, east50_2018)
plot(e50_1718_intersection, col = "green", add = TRUE)

area_e50_1718_int <-
  raster::area(e50_1718_intersection)



#want to calculate the percent overlap between these two polygons
area_e50_2017 <-
  raster::area(east50_2017)
area_e50_2018 <-
  raster::area(east50_2018)

area_de50_1718 <-
  raster::area(de50_1718) # area of 2017 polygon not covered by 2018 polygon

prop_overlap_e50_1718 <- 
  1-(area_de50_1718/area_e50_2017)

prop <- 
  area_e50_1718_int/(area_e50_2017 + area_e50_2018 - area_e50_1718_int)
  

# Calculate the other way
area_e50_2018 <-
  raster::area(east50_2018)

# area of 2018 not covered by 2017
area_de50_1817 <-
  raster::area(de50_1817)
prop_overlap_e50_1817 <-
  1-(area_de50_1817/area_e50_2018)

area_e50_2017 / area_e50_2018
# 1.023  



# calculate overlap between East 50 polygons in 2018 and 2019 -----------
plot(east50_2018)
plot(east50_2019, add = TRUE)


de50_1819<-gDifference(east50_2018, east50_2019) #this is the part of 2018 not covered by 2019
de50_1918<-gDifference(east50_2019,east50_2018)  #this is the part of 2019 not covered by 2018
plot(de50_1819, col='red', add=T) 
plot(de50_1918, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_e50_2018 <-
  raster::area(east50_2018)

area_de50_1819 <-
  raster::area(de50_1819) # area of 2018 polygon not covered by 2019 polygon

prop_overlap_e50_1819 <- 
  1-(area_de50_1819/area_e50_2018)

# Calculate the other way
area_e50_2019 <-
  raster::area(east50_2019)

# area of 2019 not covered by 2018
area_de50_1918 <-
  raster::area(de50_1918)
prop_overlap_e50_1918 <-
  1-(area_de50_1918/area_e50_2019)

area_e50_2018 / area_e50_2019
# 1.089 


# calculate overlap between East 50 polygons in 2017 and 2019 -----------
plot(east50_2017)
plot(east50_2019, add = TRUE)


de50_1719<-gDifference(east50_2017, east50_2019) #this is the part of 2017 not covered by 2019
de50_1917<-gDifference(east50_2019,east50_2017)  #this is the part of 2019 not covered by 2017
plot(de50_1719, col='red', add=T) 
plot(de50_1917, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_e50_2017 <-
  raster::area(east50_2017)

area_de50_1719 <-
  raster::area(de50_1719) # area of 2017 polygon not covered by 2019 polygon

prop_overlap_e50_1719 <- 
  1-(area_de50_1719/area_e50_2017)

# Calculate the other way
area_e50_2019 <-
  raster::area(east50_2019)

# area of 2019 not covered by 2017
area_de50_1917 <-
  raster::area(de50_1917)
prop_overlap_e50_1917 <-
  1-(area_de50_1917/area_e50_2019)

area_e50_2017 / area_e50_2019
# 1.114 






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

area_dw50_1718 <-
  raster::area(dw50_1718) # area of 2017 polygon not covered by 2018 polygon

prop_overlap_w50_1718 <- 
  1-(area_dw50_1718/area_w50_2017)

# Calculate the other way
area_w50_2018 <-
  raster::area(west50_2018)

# area of 2018 not covered by 2017
area_dw50_1817 <-
  raster::area(dw50_1817)
prop_overlap_w50_1817 <-
  1-(area_dw50_1817/area_w50_2018)

area_w50_2017 / area_w50_2018
# 0.462  



# calculate overlap between west 50 polygons in 2018 and 2019 -----------
plot(west50_2018)
plot(west50_2019, add = TRUE)


dw50_1819<-gDifference(west50_2018, west50_2019) #this is the part of 2018 not covered by 2019
dw50_1918<-gDifference(west50_2019,west50_2018)  #this is the part of 2019 not covered by 2018
plot(dw50_1819, col='red', add=T) 
plot(dw50_1918, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_w50_2018 <-
  raster::area(west50_2018)

area_dw50_1819 <-
  raster::area(dw50_1819) # area of 2018 polygon not covered by 2019 polygon

prop_overlap_w50_1819 <- 
  1-(area_dw50_1819/area_w50_2018)

# Calculate the other way
area_w50_2019 <-
  raster::area(west50_2019)

# area of 2019 not covered by 2018
area_dw50_1918 <-
  raster::area(dw50_1918)
prop_overlap_w50_1918 <-
  1-(area_dw50_1918/area_w50_2019)

area_w50_2018 / area_w50_2019
# 5.543


# calculate overlap between west 50 polygons in 2017 and 2019 -----------
plot(west50_2017)
plot(west50_2019, add = TRUE)


dw50_1719<-gDifference(west50_2017, west50_2019) #this is the part of 2017 not covered by 2019
dw50_1917<-gDifference(west50_2019,west50_2017)  #this is the part of 2019 not covered by 2017
plot(dw50_1719, col='red', add=T) 
plot(dw50_1917, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_w50_2017 <-
  raster::area(west50_2017)

area_dw50_1719 <-
  raster::area(dw50_1719) # area of 2017 polygon not covered by 2019 polygon

prop_overlap_w50_1719 <- 
  1-(area_dw50_1719/area_w50_2017)

# Calculate the other way
area_w50_2019 <-
  raster::area(west50_2019)

# area of 2019 not covered by 2017
area_dw50_1917 <-
  raster::area(dw50_1917)
prop_overlap_w50_1917 <-
  1-(area_dw50_1917/area_w50_2019)

area_w50_2017 / area_w50_2019
# 2.564



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

area_de95_1718 <-
  raster::area(de95_1718) # area of 2017 polygon not covered by 2018 polygon

prop_overlap_e95_1718 <- 
  1-(area_de95_1718/area_e95_2017)

# Calculate the other way
area_e95_2018 <-
  raster::area(east95_2018)

# area of 2018 not covered by 2017
area_de95_1817 <-
  raster::area(de95_1817)
prop_overlap_e95_1817 <-
  1-(area_de95_1817/area_e95_2018)

area_e95_2017 / area_e95_2018
# 1.325  



# calculate overlap between east 95 polygons in 2018 and 2019 -----------
plot(east95_2018)
plot(east95_2019, add = TRUE)


de95_1819<-gDifference(east95_2018, east95_2019) #this is the part of 2018 not covered by 2019
de95_1918<-gDifference(east95_2019,east95_2018)  #this is the part of 2019 not covered by 2018
plot(de95_1819, col='red', add=T) 
plot(de95_1918, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_e95_2018 <-
  raster::area(east95_2018)

area_de95_1819 <-
  raster::area(de95_1819) # area of 2018 polygon not covered by 2019 polygon

prop_overlap_e95_1819 <- 
  1-(area_de95_1819/area_e95_2018)

# Calculate the other way
area_e95_2019 <-
  raster::area(east95_2019)

# area of 2019 not covered by 2018
area_de95_1918 <-
  raster::area(de95_1918)
prop_overlap_e95_1918 <-
  1-(area_de95_1918/area_e95_2019)

area_e95_2018 / area_e95_2019
# 1.694


# calculate overlap between east 95 polygons in 2017 and 2019 -----------
plot(east95_2017)
plot(east95_2019, add = TRUE)


de95_1719<-gDifference(east95_2017, east95_2019) #this is the part of 2017 not covered by 2019
de95_1917<-gDifference(east95_2019,east95_2017)  #this is the part of 2019 not covered by 2017
plot(de95_1719, col='red', add=T) 
plot(de95_1917, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_e95_2017 <-
  raster::area(east95_2017)

area_de95_1719 <-
  raster::area(de95_1719) # area of 2017 polygon not covered by 2019 polygon

prop_overlap_e95_1719 <- 
  1-(area_de95_1719/area_e95_2017)

# Calculate the other way
area_e95_2019 <-
  raster::area(east95_2019)

# area of 2019 not covered by 2017
area_de95_1917 <-
  raster::area(de95_1917)
prop_overlap_e95_1917 <-
  1-(area_de95_1917/area_e95_2019)

area_e95_2017 / area_e95_2019
# 2.244


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

area_dw95_1718 <-
  raster::area(dw95_1718) # area of 2017 polygon not covered by 2018 polygon

prop_overlap_w95_1718 <- 
  1-(area_dw95_1718/area_w95_2017)

# Calculate the other way
area_w95_2018 <-
  raster::area(west95_2018)

# area of 2018 not covered by 2017
area_dw95_1817 <-
  raster::area(dw95_1817)
prop_overlap_w95_1817 <-
  1-(area_dw95_1817/area_w95_2018)

area_w95_2017 / area_w95_2018
# 0.958  



# calculate overlap between east 95 polygons in 2018 and 2019 -----------
plot(west95_2018)
plot(west95_2019, add = TRUE)


dw95_1819<-gDifference(west95_2018, west95_2019) #this is the part of 2018 not covered by 2019
dw95_1918<-gDifference(west95_2019,west95_2018)  #this is the part of 2019 not covered by 2018
plot(dw95_1819, col='red', add=T) 
plot(dw95_1918, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_w95_2018 <-
  raster::area(west95_2018)

area_dw95_1819 <-
  raster::area(dw95_1819) # area of 2018 polygon not covered by 2019 polygon

prop_overlap_w95_1819 <- 
  1-(area_dw95_1819/area_w95_2018)

# Calculate the other way
area_w95_2019 <-
  raster::area(west95_2019)

# area of 2019 not covered by 2018
area_dw95_1918 <-
  raster::area(dw95_1918)
prop_overlap_w95_1918 <-
  1-(area_dw95_1918/area_w95_2019)

area_w95_2018 / area_w95_2019
# 2.357


# calculate overlap between east 95 polygons in 2017 and 2019 -----------
plot(west95_2017)
plot(west95_2019, add = TRUE)


dw95_1719<-gDifference(west95_2017, west95_2019) #this is the part of 2017 not covered by 2019
dw95_1917<-gDifference(west95_2019,west95_2017)  #this is the part of 2019 not covered by 2017
plot(dw95_1719, col='red', add=T) 
plot(dw95_1917, col='blue', add=T)

#want to calculate the percent overlap between these two polygons
area_w95_2017 <-
  raster::area(west95_2017)

area_dw95_1719 <-
  raster::area(dw95_1719) # area of 2017 polygon not covered by 2019 polygon

prop_overlap_w95_1719 <- 
  1-(area_dw95_1719/area_w95_2017)

# Calculate the other way
area_w95_2019 <-
  raster::area(west95_2019)

# area of 2019 not covered by 2017
area_dw95_1917 <-
  raster::area(dw95_1917)
prop_overlap_w95_1917 <-
  1-(area_dw95_1917/area_w95_2019)

area_w95_2017 / area_w95_2019
# 2.258

# Make table of overlap values

e50_overlap <- 
  data.frame(year = 2017:2019,
             y2017 = round(c(prop_overlap_e50_1718, 1, prop_overlap_e50_1719), 2),
             y2018 = round(c(1, prop_overlap_e50_1817, prop_overlap_e50_1819), 2), 
             y2019 = round(c(prop_overlap_e50_1917, prop_overlap_e50_1918, 1), 2))

# write.table
write_csv(e50_overlap, "results/east50_overlap.csv")


w50_overlap <- 
  data.frame(year = 2017:2019,
             y2017 = round(c(prop_overlap_w50_1718, 1, prop_overlap_w50_1719), 2),
             y2018 = round(c(1, prop_overlap_w50_1817, prop_overlap_w50_1819), 2), 
             y2019 = round(c(prop_overlap_w50_1917, prop_overlap_w50_1918, 1), 2))

# write.table
write_csv(w50_overlap, "results/west50_overlap.csv")


e95_overlap <- 
  data.frame(year = 2017:2019,
             y2017 = c(1, prop_overlap_e95_1817, prop_overlap_e95_1917), 
             y2018 = c(prop_overlap_e95_1718, 1, prop_overlap_e95_1918),
             y2019 = c(prop_overlap_e95_1719, prop_overlap_e95_1819, 1))

# write.table
write_csv(e95_overlap, "results/east95_overlap.csv")


w95_overlap <- 
  data.frame(year = 2017:2019,
             y2017 = c(1, prop_overlap_w95_1817, prop_overlap_w95_1917), 
             y2018 = c(prop_overlap_w95_1718, 1, prop_overlap_w95_1918),
             y2019 = c(prop_overlap_w95_1719, prop_overlap_w95_1819, 1))

# write.table
write_csv(w50_overlap, "results/west95_overlap.csv")
