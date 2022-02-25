# Function to summarize molt locations and make figures of summarized data ####
# requires that you have MPA, 2000m iso, Antarctica coastline, and ACC layers already loaded

# future improvements will add those to function?

colony="CROZ"
seasons=2016
grid_size=50000

plot_mlocs <- function(data,colony,seasons,months=unique(data$month),grid_size,xlab,
                       ylab,legend.title,legend.position,rast_path=NULL,poly_path=NULL,scaled_rast=TRUE,title=""){
  # func=match.fun(fun)
  # filter data to subset desired
  data_sub <- data%>%
    filter(br_col%in%colony,
           season%in%seasons,
           month%in%months)

  
  # Create 50 km bins so can summarize data by bin
  
  # function to round down to the nearest grid size meters
  mround <- function(x,grid_size){
    grid_size*floor(x/grid_size)
  }
  
  
  # add column for x and y bin
  data_sub$x_bin <-mround(data_sub$x,grid_size)
  data_sub$y_bin <- mround(data_sub$y,grid_size)
  
  # summarize total foraging dives per grid ####
  data_summ <-data_sub%>%
    group_by(x_bin,y_bin)%>%
    summarise(n_locs=n(),.groups="drop")%>%
    as.data.frame()
  
  r<-raster(extent(c(xmin=(min(data_summ$x_bin)-grid_size*2),xmax=(max(data_summ$x_bin)+grid_size*2),
             ymin=(min(data_summ$y_bin)-grid_size*2),ymax=(max(data_summ$y_bin)+grid_size*2))),
            crs=proj_ant,resolution=grid_size)
  
  summ_grid <- rasterize(data_summ[,c("x_bin","y_bin")],y=r,field=data_summ$n_locs)
  # writeRaster(summ_grid,path,format="GTiff",overwrite=T)
  # scale to 0 to 1
  # divide by the value range to rescale to 0,1 
  scl <- function(x) {
    (x - min(x,na.rm = TRUE)) / diff(range(x, na.rm = TRUE))
  }
  
  summ_grid_scl <-setValues(summ_grid, scl(values(summ_grid)))
  
  if(!is.null(rast_path)){
  writeRaster(summ_grid_scl,rast_path,format="GTiff",overwrite=T)
  }else{
    message("no raster path provided, raster not saved")
  }
  
  # convert to SPDF to be able to plot in ggplot 
  if(scaled_rast){
    summ_SPixDF<-as.data.frame(as(summ_grid_scl,"SpatialPixelsDataFrame"))
  }else{
    summ_SPixDF<-as.data.frame(as(summ_grid,"SpatialPixelsDataFrame"))
  }
  # get contour lines
  cont_50 <- rasterToContour(summ_grid_scl,levels=0.5)
  
  cont_95 <- rasterToContour(summ_grid_scl,levels=0.05)

# convert spatial lines to polygons  
  if (!is.null(poly_path)){
  # first convert contours to polygons
  cont_50_polys=list()
   for(i in 1:length(cont_50@lines[[1]]@Lines)){
    cont_50_polys[[i]]<- Polygon(coords=cont_50@lines[[1]]@Lines[[i]]@coords)
    cont_50_polys[[i]]@hole = FALSE
   }

  # then convert to list of polygons
  cont_50_poly_ls <- Polygons(cont_50_polys,"cont_50")
  # then convert to spatial Polygons
  cont_50_poly_sp <-SpatialPolygons(list(cont_50_poly_ls), proj4string = proj_ant)

  # clip to coastline
  cont_50_poly_clip <- cont_50_poly_sp-ant_clip
  
  # then convert to sf and remove holes
  cont_50_poly_sf <- st_remove_holes(st_as_sf(cont_50_poly_clip))
  
  #convert 95% contours to polygons
  cont_95_polys=list()
  for(i in 1:length(cont_95@lines[[1]]@Lines)){
    cont_95_polys[[i]]<- Polygon(coords=cont_95@lines[[1]]@Lines[[i]]@coords)
    cont_95_polys[[i]]@hole = FALSE
  }

  # # then conver to list
  cont_95_poly_ls <- Polygons(cont_95_polys,"cont_50")
  # then convert to spatial Polygons
  cont_95_poly_sp <-SpatialPolygons(list(cont_95_poly_ls), proj4string = proj_ant)
  # clip to coastline
  cont_95_poly_clip <- cont_95_poly_sp-ant_clip
  # then convert to sf and remove holes
  cont_95_poly_sf <- st_remove_holes(st_as_sf(cont_95_poly_clip))
  # plot(cont_95_poly_clip)


  
    write_sf(cont_50_poly_sf,paste(poly_path,50,"poly.shp",sep="_"),driver="ESRI Shapefile")
    write_sf(cont_95_poly_sf,paste(poly_path,95,"poly.shp",sep="_"),driver="ESRI Shapefile")
  }else{
    message("no polygon path provided, polygon not saved")
  }
  
  # # convert polylines to simple features for plotting
  # cont_50 <- cont_50%>%
  #   {. ->> contours_sp} %>%
  #   st_as_sf %>%
  #   {. ->> contours_sf}
  # 
  # cont_95 <-cont_95  %>%
  #   {. ->> contours_sp} %>%
  #   st_as_sf %>%
  #   {. ->> contours_sf}

  

 p<- ggplot()+
    geom_tile(data=summ_SPixDF,aes(x,y,fill=layer))+
    scale_fill_viridis(legend.title)+
    # 1000m isobath
    geom_path(data=iso1000,aes(x = long, y = lat,group=group,col="1000m isobath"),show.legend = TRUE,size=0.75)+
    # add 50% and 95% contours
    geom_path(data=cont_50_poly_clip,size=1,aes(x=long,y=lat, group=group,col="50%"),show.legend = "line")+
    geom_path(data=cont_95_poly_clip,size=1,aes(x=long,y=lat, group=group,col="95%"), show.legend = "line")+
   # mpa boundary
   geom_polygon(data=mpa_t,aes(x=long,y=lat, group=group,col="RSRMPA",linetype = "RSRMPA"),
                show.legend = "line",fill="grey",alpha=0,size=0.8)+
 
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
      xlim = c(-1625000,   2075000),
      ylim = c(825000, 3175000),
    )+
    theme_classic()+
    scale_color_manual("",values=c("50%"="green", "95%"="purple","RSRMPA" = "grey85","1000m isobath" = "grey50"),
                       breaks = c("50%", "95%","RSRMPA","1000m isobath"))+
    # scale_linetype_manual(values =c("50%"=1, "95%"=1,"RSRMPA" = 1,"2000m isobath" = 1,"ACC front" = 3))+
    guides(color = guide_legend(override.aes = list(linetype = c(1,1,1,1))))+
    scale_linetype(guide = FALSE)+
    theme(title = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 90),
          legend.title = element_text(size = 10))+
    xlab(xlab) +
    ylab(ylab) +
    # scale_fill_manual(lims=c(0.0001,1))+
    scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))+
    ggtitle(title)
  # c(seq(110,180,by=10),seq(-170,-100,by=10)))
  print(p)
  return(p)
}
