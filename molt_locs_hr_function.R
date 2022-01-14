# function to calculate kernal UD and output figure and HR contours

# Function to summarize molt locations and make figures of summarized data ####
# requires that you have MPA, 2000m iso, Antarctica coastline, and ACC layers already loaded

# future improvement will add those to function
plot_molt_hr <- function(data,colony,seasons,bid = unique(data$bird_id),months=unique(data$month),sex,grid_size,xlab,
                       ylab,legend.title,legend.position,rast_path,poly_path,scaled_rast=TRUE,title=""){

  
  data = molt_locs%>%
    mutate(month = month(time1))
  colony = "ROYD"
  seasons = 2017:2019
  months = unique(data$month)
  
  # filter data to subset desired
  data_sub <- data%>%
    filter(bird_id%in%bid,
      br_col%in%colony,
           season%in%seasons,
           month%in%months)%>%
    mutate(season=season+1)%>%
    unite("uniq_id", bird_id,season)%>%
    # mutate(uniq_id = factor(uniq_id))
    mutate(uniq_id=factor(1))%>%
    filter(lat>(-90))

  
  
  data_SP <- SpatialPointsDataFrame(coords = data_sub[,c("lon","lat")], data = data_sub[,"uniq_id"])
  data_SP <- SpatialPoints(coords = data_sub[,c("lon","lat")])
  
  ,data=data_sub[,"uniq_id"],
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +lon_wrap"))
  
  UD <- adehabitatHR::kernelUD(data_SP,h = "LSCV",grid=50)
  cont_95 <- adehabitatHR::getverticeshr(UD,percent = 95)
  cont_50 <- adehabitatHR::getverticeshr(UD,percent = 50)
  crs(cont_95) <- "+proj=longlat +ellps=WGS84 +lon_wrap"
  crs(cont_50) <- "+proj=longlat +ellps=WGS84 +lon_wrap"
  writeOGR(cont_95,dsn="Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/contours",layer="test4_hr",driver= "ESRI Shapefile")
  writeOGR(cont_50,dsn="Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/contours",layer="test50_hr",driver= "ESRI Shapefile")
  
  image(UD)
  
  
  p <- ggplot()+
    geom_tile(data=summ_SPixDF,aes(x,y,fill=layer))+
    scale_fill_viridis(legend.title)+
    # 2000m isobath
    # 2000m isobath
    geom_path(data=iso2000,aes(x = long, y = lat,group=group,col="2000m isobath"),show.legend = "line",size=0.75)+
    # acc front
    # geom_path(data=front,aes(x = long, y = lat,group=group, col = "ACC front"),show.legend = "line",size=0.8, lty=3)+
    # mpa boundary
    geom_polygon(data=mpa_t,aes(x=long,y=lat, group=group,col="RSRMPA",linetype = "RSRMPA"),show.legend = "line",fill="grey",alpha=0,size=0.8)+
    # add 50% and 95% contours
    geom_sf(data=cont_50,size=1,aes(color="50%"),show.legend = "line")+
    geom_sf(data=cont_95,size=1.2,aes(color = "95%"), show.legend = "line")+
    
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
    )+
    theme_classic()+
    scale_color_manual("",values=c("50%"="green", "95%"="purple","RSRMPA" = "grey85","2000m isobath" = "grey50"),
                       breaks = c("50%", "95%","RSRMPA","2000m isobath"))+
    # scale_linetype_manual(values =c("50%"=1, "95%"=1,"RSRMPA" = 1,"2000m isobath" = 1,"ACC front" = 3))+
    guides(color = guide_legend(override.aes = list(linetype = c(1,1,1,1))))+
    scale_linetype(guide = FALSE)+
    theme(title = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 90),
          legend.title = element_text(size = 10))+
    # scale_color_manual(values=c(", "blue"), 
    #                    labels=c("route 1", "route 2"))
    xlab(xlab) +
    ylab(ylab) +
    # scale_fill_manual(lims=c(0.0001,1))+
    scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))+
    ggtitle(title)
  # c(seq(110,180,by=10),seq(-170,-100,by=10)))
  # print(p)
  return(p)
}
