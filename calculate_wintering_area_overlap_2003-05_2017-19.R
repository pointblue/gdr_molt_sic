#Assesses difference between 2003-2005 "winter polygon" from Ballard et al. 2010 Ecology
#and 2017-2019 95% locations polygon from GDR study
#
#Grant
#original version: 11/17/2021
#last update: 11/21/2021


##Specify the GIS directory you are working from (depends on what network you are on)
GIS_dir<-"GIS"

##Load  any needed libraries####
library(rgdal) #for readOGR, spTransform
library(rgeos)##for gDifference
library(raster) #for area
library(sf) #for converting spatialpolygons to sf for writing final output
#these only needed for the sample size calcs part: 
library(dplyr) #note that you mask some potentially important commands from raster and rgeos here (union, intersect etc)
library(data.table) #for fread - only need dplyr and readr for calculating sample sizes for new_polygon
library(sqldf)
#library(dplyr)
#library(adehabitatHR)
#library(sp)
#library(ggplot2) #for plotting results
#library(pastecs) #for stat.desc

#---------------------------------------#
#define projection we are working in ####
#---------------------------------------#
the_crs <-"+proj=lcc +lat_1=-76.666667 +lat_2=-79.333333 +lat_0=-78.021171 +lon_0=169.333333 
+x_0=500000 +y_0=300000 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#-----------------------------------------------------#
#get the Antarcic coastline for clipping, if needed ####
#-----------------------------------------------------#
coastline<-readOGR("GIS/ADDcstpolyline_edit_2021_11_23.shp")
coastline<-spTransform(coastline, the_crs)

#-------------------------------------------------------------------------------------------------#
#documentation for numbers of individuals per season x colony for 2003-2005 polygon calculation####
#-------------------------------------------------------------------------------------------------#

#Based on data from 41 retrieved tags in the 2003-2005 (Ballard et al. 2010) study; 
8+8+10
#26 from Crozier
3+5+7
#15 from Royds

#-------------------------------------------------------------------------------------------------#
#documentation for numbers of individuals per season x colony for 2017-2019 polygon calculation####
#-------------------------------------------------------------------------------------------------#
#This is based on these individuals (from map_all_nonbreed_locs.R; WARNING: 3.8GB):
 locs500<- fread("data/gdr_locs_final_500_all_yr_v2021-06-03.csv",nThread=8)%>%
    mutate(doy=as.numeric(format(time1,"%j")))%>%
    filter(!doy%in%c(86:109)&!doy%in%c(229:269))

length(unique(locs500$bird_fn))

#120 individuals in this file
#get number of individuals x seasons
a<-sqldf("select distinct bird_fn, season from locs500")
#208 total 
length(unique(a$bird_fn))

#to get it by colony we need the deploy file:
b<-read.csv("data/croz_royds_gdr_depl_all_v2021-08-27.csv")
length(unique(b$bird_fn))

c <- sqldf("select a.*, b.br_col from a a, b b 
   where a.bird_fn=b.bird_fn
   group by a.bird_fn, a.season")

#208 deployments total - several of which were on the same individuals
length(unique(c$bird_fn))
#120 individuals total
length(unique(c$season))
#3 seasons
length(unique(c$br_col))
#2 colonies
#
n_birds_cs<-sqldf("select season, br_col, count(bird_fn) as count from c group by season,br_col")
#season colony  n
#2016   CROZ    53
#2017   CROZ    61
#2018   CROZ    45

#2016   ROYD    13
#2017   ROYD    18
#2018   ROYD    18
sum(n_birds_cs$count)
#208 birds - matches above

query<-"select bird_fn, season, br_col 
  from gdr_birds_df 
  where locs_filename!='NA' AND data='c' 
  group by bird_fn, br_col, season"
d<-sqldf(query)
#185 deployments marked as having complete data - so 23 in the sample that didn't have complete data
length(unique(d$bird_fn))
#111 individuals
#going to re-run to only use these 185
#who are these birds (bird_fn and season) that need to be eliminated?

d$bird_seas<-paste0(d$bird_fn,d$season)
c$bird_seas<-paste0(c$bird_fn,c$season)

drop<-sqldf("select distinct c.bird_fn, c.season, c.bird_seas from c c 
            where c.bird_seas not in 
            (select d.bird_seas from d d)")

#-----------------------------------------------------------------------------------------#
#Create a nicer version of the 2017-19 home range - added the coastline to it in ArcMap####
#-----------------------------------------------------------------------------------------#
new_polygon<-readOGR("data/contours/CROZ_ROYD_nonbreed_locs_all_95_poly.shp")
new_polygon<-spTransform(new_polygon, the_crs)
plot(new_polygon)

#this is the home range plus extra to allow for clipping to the coastline (made in ArcMap)
area_to_be_clipped<-readOGR("data/contours/CROZ_ROYD_nonbreed_locs_all_95_poly_edit.shp")
area_to_be_clipped<-spTransform(area_to_be_clipped, the_crs)
plot(area_to_be_clipped)

#this is an edit of the coastline reflecting recent satellite imagery / changes to Ross Ice Shelf and other Ross Sea ice features
coastline<-readOGR("GIS/ADDcstpolyline_edit_2021_11_23.shp")
#coastline<-readOGR("Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/GIS/ADDcstpoly_outline_Clip_shape.shp")
coastline<-spTransform(coastline, the_crs)
plot(coastline, add=T)

#Convert coastline (SpatialLinesDataFrame) to a SpatialPolygon####
#First convert lines to polygons
coast_polys=list()
for(i in 1:length(coastline@lines)){
  coast_polys[[i]]<- Polygon(coords=coastline@lines[[i]]@Lines[[1]]@coords)
  coast_polys[[i]]@hole = FALSE
}
# then convert to list
coast_polys_ls <- Polygons(coast_polys,"coast_polys")

# then convert to SpatialPolygons
coast_polys_sp <-SpatialPolygons(list(coast_polys_ls), proj4string = crs(the_crs))

#coast_polys_sp@polygons[[1]]@Polygons[[11]]
#plot(coast_polys_sp)

#Clip home range area and write####
new_area<-area_to_be_clipped-coast_polys_sp #I really can't believe this works (only if both SpatialPolygons though)
plot(new_area)
writeOGR(new_area, dsn=paste0(analyses_dir,"data/contours"), "home_range_2017-19_cst", driver="ESRI Shapefile", overwrite_layer = T)

plot(new_area, col=('yellow'))
plot(coastline, add=T, col=('black'))


#-------------------------------------#
#get the polygons from both studies####
#-------------------------------------#

old_polygon<-readOGR("data/contours/WinterArea_2003_2005/WinterPolygon2003-2005.shp")
new_polygon<-readOGR("data/contours", "home_range_2017-19_cst")


old_polygon<-spTransform(old_polygon, the_crs)  
new_polygon<-spTransform(new_polygon, the_crs)


plot(new_polygon)
plot(old_polygon, add=T)
#plot(barrier2, add=T)

d1<-gDifference(old_polygon,new_polygon)
d2<-gDifference(new_polygon,old_polygon)
plot(d1, col='red', add=T) #this is the part of old_polygon not covered by new_polygon
plot(d2, col='blue', add=T) #this is the part of new_polygon not covered by old_polygon

#want to calculate the percent overlap between these two polygons
#new_polygon is bigger than old_polygon; calc percent of old_polygon that is covered by new_polygon

area_op<-area(old_polygon) #area of original polygon
area_d1<-area(d1) #area of olod polygon not covered by new polygon
prop_overlap<-1-(area_d1/area_op) #this is the proportion of the original polygon covered by the new polygon
#0.89

#now calculate the other way
area_np<-area(new_polygon) #area of new polygon
area_d2<-area(d2) #area of new polygon not covered by old polygon
prop_overlap_2<-1-(area_d2/area_np) #this is the proportion of the new polygon covered by the old polygon
#0.83

area_op / area_np
#0.94

#next join the old_polygon and new_polygon to make one shape to rule them all
bound_polygon<-bind(old_polygon, new_polygon)
plot(bound_polygon, add=T)
#Double check this - makes it into a SpatialPolygons (loses the DataFrame part) and also is not inclusive of outlying
#polygons in the new_polygon...
combined_polygon<-aggregate(bound_polygon, fun=max, dissolve=T)
plot(combined_polygon)
area(combined_polygon)
3.14e+12/1000^2
#3,140,000 #double check! (square km)
prop_overlap_3<-area_op/area(combined_polygon) #this is the percent of the combined polygon that is comprised of old_polygon
prop_overlap_4<-area_np/area(combined_polygon) #this is the percent of the combined polygon that is comprised of new_polygon

#write the final result####
#need to convert to SpatialPolygonsDataFrame before this will work:
combined_polygon_spdf<-SpatialPolygonsDataFrame(combined_polygon,data=data.frame(id=1))
writeOGR(combined_polygon_spdf, dsn=paste0(analyses_dir,"data/contours"), "WinterPolygon2003-05_2017-19", driver="ESRI Shapefile", overwrite_layer = T)

