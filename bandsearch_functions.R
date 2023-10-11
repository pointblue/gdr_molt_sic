#00000000000000000000000
####seas_fy function####
#00000000000000000000000
seas_fy <- function(s) {
  #  s here is like "1920" for 2019 - 2020; returns 4 digit year
  y1 <- substring(s, 1, 2)
  if (as.numeric(y1) > 90) {
    sfy <- paste0("19", y1)
  }
  else {
    sfy <- paste0("20", y1)
  }
  #RETURN season_fullyr
  return(as.numeric(sfy))
}
#00000000000000000000000
####end seas_fy function
#00000000000000000000000

#00000000000000000000000
####jdate function####
#00000000000000000000000
jdate <- function(md) {
  #returns day of year
  #if md is a character string containing a date in "mm/dd/yyyy" format
  #reformat to R-friendly form (yyyy-mm-dd)
  #md<-paste0(substr(md,7,10),"-",substr(md,1,2),"-",substr(md,4,5))
  return(format(md, "%j"))
}
#00000000000000000000000
####end jdate function
#00000000000000000000000

#00000000000000000000000000000
####season_fullyr function####
#00000000000000000000000000000
#Returns full first year of season (4 digits)
#from a date as character formatted "yyyy-mm-dd"
season_fullyr <- function(md) {
  if (!is.na(md)) {
    if (jdate(md) > 305) {
      return(as.numeric(substr(md, 1, 4)))
    } else {
      return(as.numeric(substr(md, 1, 4)) - 1)
    }
  } else {
    return(NA)
  }
}
#000000000000000000000000000000000
####end season_fullyr function####
#000000000000000000000000000000000

#00000000000000000000000000000
####Breeder function####
#00000000000000000000000000000
#returns breeding flag; 1=breeder 0=no evidence of breeding
breeder <- function(x, ne, nc) {
  if (is.na(x[ne])) {
    x[ne] <- 0
  }
  if (is.na(x[nc])) {
    x[nc] <- 0
  }
  if ((as.numeric(x[ne]) > 0 &
       as.numeric(x[ne] < 9)) |
      (as.numeric(x[nc]) > 0 & as.numeric(x[nc]) < 9)) {
    br <- 1
  } else {
    br <- 0
  }
  return(br)
}


#00000000000000000
#####GPS nest function with DPLYR
#0000000000

gpsd_locs <-
  function(selection, # "all", "noM", "M_only"
           season, # current season's start year, aka 2122 = 2021
           status, # "breeders", "active", "all"
           cr_check = "no",
           filename = "",
           include_ty = TRUE) {
    require(rgdal)
    require(dplyr)
    require(maptools)
    thisseason = season
    lastseason = thisseason - 1
    lastseason2 = thisseason - 2
    
    #you might have already done these things but you need to do them for this to work:
    allrs$season_yr <- as.numeric(lapply(allrs$DATE, season_fullyr))
    allrs$BANDNUMB <- as.numeric(allrs$BANDNUMB)
    
    
    # First get last 2 years GPS, options are either active or all
    
    if (status == "breeders") { #| status == "active"??) {
      lastyrbreed <- allrs %>%
        filter((season_yr == lastseason |
                    season_yr == lastseason2)
               & ((NUCH > 0 & NUCH < 9) | (NUEGG > 0 & NUEGG < 9)))
      
      #to get GPS from breeders in last two seasons
      lastyrgps <- lastyrbreed %>%
        filter(LAT != 0) %>%
        group_by(season_yr, BANDNUMB) %>%
        arrange(DATE) %>%
        slice(n())
    }
    if (status == "all") {
      lastyrgps <- allrs %>%
        filter((season_yr == lastseason |
                  season_yr == lastseason2) & LAT != 0) %>%
        group_by(season_yr, BANDNUMB) %>%
        arrange(DATE) %>%
        slice(n())
    }
    
    
    # if doing this before have any data from this season, set include_ty to FALSE
    if (include_ty) {
      rsfile$season_yr <- thisseason
      #to get active breeders this year
      thisyrbreed <- rsfile %>%
        filter((NUCH < 9 & NUCH > 0) | (NUEGG > 0 & NUEGG < 9))%>%
        group_by(BANDNUMB)%>%
        slice(n())%>%
        select(BANDNUMB, LOCATION, SUBCOLONY, INITIALS)
      
      if (status == "breeders" | status == "active") {
        #this season GPS points for breeders
          thisyrgps <- rsfile %>%
          filter(BANDNUMB %in% thisyrbreed$BANDNUMB, LAT != 0)%>%
          group_by(BANDNUMB)%>%
          slice(n())
      }
      
      # otherwise include all GPS coords from this season (active or not active)
      if (status == "all") {
        thisyrgps <- rsfile %>%
          filter(LAT != 0) %>%
          group_by(BANDNUMB) %>%
          arrange(DATE) %>%
          slice(n())
      }
      
      
      #check all active nests have GPS points
      check_gps <-
        anti_join(thisyrbreed, thisyrgps, by = "BANDNUMB") %>%
        select(c(BANDNUMB, LOCATION, SUBCOLONY, INITIALS))
      
      # list of failed nests
      failed_nests <- rsfile %>%
        filter(BANDNUMB %in% thisyrbreed$BANDNUMB) %>%
        group_by(BANDNUMB) %>%
        filter(STATUS == "FAIL" | STATUS == "P/FAIL") %>%
        arrange(DATE) %>%
        slice(n()) %>%
        select(BANDNUMB)
      
      # create data frame of gps coords including this year and last 2 years
      if (status == "active") {
        gpsd_nests <- thisyrgps
      } else {
        gpsd_nests <- suppressWarnings(bind_rows(lastyrgps, thisyrgps))
      }
      
      
      #group by BANDNUMB
      #arrange descending by season
      # select only most recent location for each bandnumber
      gpsd_nests <- gpsd_nests %>%
        group_by(BANDNUMB) %>%
        arrange(desc(DATE)) %>%
        slice(1) %>%
        mutate(SEASON = season_yr) %>%
        select(-season_yr)
      
      # filters based on last nest status
      if (status == "active" & nrow(failed_nests) > 0) {
        gpsd_nests <- gpsd_nests %>%
          filter(BANDNUMB %in% thisyrgps$BANDNUMB,!BANDNUMB %in% failed_nests$BANDNUMB)
      }
      if (status == "active" & nrow(failed_nests == 0)) {
        gpsd_nests <- gpsd_nests %>%
          filter(BANDNUMB %in% thisyrgps$BANDNUMB)
      } # else {
      #   gpsd_nests <- gpsd_nests
      # }
      
      
      if (cr_check == "yes") {
        creched_nests <- rsfile %>%
          filter(BANDNUMB %in% thisyrbreed$BANDNUMB) %>%
          group_by(BANDNUMB) %>%
          filter(STATUS == "CR?" |
                   STATUS == "P/CR?" |
                   STATUS == "CR" | STATUS == "CR9?" |
                   STATUS == "P/CR" | STATUS == "CR?/ABD") # %>%
          # tally() %>%
          # filter(n > 1)   ## not sure why this is here - DK 1/3/22
        
        last_status <- rsfile %>%
          filter(
            BANDNUMB %in% thisyrgps$BANDNUMB &
              !BANDNUMB %in% creched_nests$BANDNUMB &
              !BANDNUMB %in% failed_nests$BANDNUMB
          ) %>%
          group_by(BANDNUMB) %>%
          arrange(DATE) %>%
          slice(n()) %>%
          select(BANDNUMB, LOCATION, DATE, STATUS)
        
        gpsd_nests <- gpsd_nests %>%
          filter(
            # BANDNUMB %in% thisyrgps$BANDNUMB &    ## this shouldn't be here?
              !BANDNUMB %in% creched_nests$BANDNUMB &
              !BANDNUMB %in% failed_nests$BANDNUMB
          ) %>%
          select(BANDNUMB, LAT, LON, LOCATION, DATE, SEASON) %>%
          left_join(last_status) 
        # code to add the most recent status doesn't work and I don't know why
        # %>%
        #   mutate(STATUS2 = as.character(last_status$STATUS))
          
                 # STATUS = as.character(STATUS),
                 # STATUS = ifelse(STATUS == "CR9?", "CR?", STATUS))
      }
      
      if (status == "all") {
        gpsd_nests <- gpsd_nests
      }
      
      
    } else {
      # if not including this year:
      gpsd_nests <- lastyrgps
      #group by BANDNUMB
      #arrange descending by season
      gpsd_nests <- gpsd_nests %>%
        group_by(BANDNUMB) %>%
        arrange(desc(DATE)) %>%
        slice(1) %>%
        mutate(SEASON = season_yr) %>%
        select(-season_yr)
      
      check_gps = "no GPS coords from this yr"
    }
    
    # Filters based on spatial selection
    if (selection == "noM") {
      gpsd_nests <- gpsd_nests %>%
        filter(LOCATION != "M")
    }
    
    if (selection == "M_only") {
      gpsd_nests <- gpsd_nests %>%
        filter(LOCATION == "M")
    }
    
    # 
    # if(ty_only){
    #   gpsd_nests=thisyrgps%>%
    #     filter(BANDNUMB%in%thisyrbreed$BANDNUMB)
    # }
    # 
    #make shapefile with gps points
    # names <- c("07_BG","10_CH1SIZE","11_CH2SIZE","08_NP","12_NOTES","04_NUCH","03_NUEGG","05_SEX",
               # "06_SEXHOW","02_STATUS","09_SUBCOL")
    # BANDNUMB <- gpsd_nests$BANDNUMB
    
    
    # the_data <- as.data.frame(matrix(NA,length(BANDNUMB),length(names)))%>%
      # mutate_all(as.character)
      # mutate("07_BG"=factor(NA,levels=c(1,2,3)))
    
    # names(the_data)=names
    # the_data$BANDNUMB <- as.character(BANDNUMB)
    # the_data$name <- as.character(BANDNUMB)
    the_data = gpsd_nests
    # the_data$LON <-  gpsd_nests$LON
    # the_data$LAT <-gpsd_nests$LAT
    #xy=gpsd_nests<-gpsd_nests[c("LON", "LAT")]
    coordinates(gpsd_nests) = c("LON", "LAT")
    proj4string(gpsd_nests) <- CRS(crsWGS84)
    
    the_shape <- SpatialPointsDataFrame(gpsd_nests, data = the_data)
    
    writeOGR(
      the_shape,
      dsn = "../GIS/bandsearch_shapefiles",
      paste("gpsd_nests", selection, status, filename, sep = "_"),
      driver = "ESRI Shapefile",
      overwrite_layer = T
    )
    
    # compress file
    fn <- paste("../GIS/bandsearch_shapefiles/gpsd_nests_", selection, status, filename, sep = "_")
    shape_files <- c(paste0(fn,".dbf"),paste0(fn,".prj"),paste0(fn,".shp"),paste0(fn,".shx"))
    # shape_files <- c(paste0(fn,".dbf"),paste0(fn,".prj")
    
    # zip::zip("../GIS/bandsearch_shapefiles/2021-11-25.zip", files = shape_files)
    
    # maptools::kmlPoints(the_shape,kmlfile = paste("../GIS/bandsearch_shapefiles/", "gpsd_nests",selection,filename,sep="_"))
    # raster::KML(the_shape,paste("../GIS/bandsearch_shapefiles/", "gpsd_nests",selection,filename,".kml",sep="_"))
    if (include_ty) {
      if (nrow(check_gps) > 0) {
        message("ACTIVE NESTS THAT STILL NEED GPS COORDINATES:")
        return(check_gps)
      }
    }
  }
