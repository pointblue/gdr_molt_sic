# Description -------------------------------------------------------------
# Code to calculate annual return rates
# Creates table resight_summary.csv
# currently set up to exclude Pre-breeders but previously run to include them
# First drafted
# November 13, 2021
# Grant and Annie


# Setup -------------------------------------------------------------------

#get the packages you need####
library(foreign)
library(tidyverse)
library(sqldf)


#set the working directory####
setwd("Z:/Informatics/S031/S0312122/croz2122/bandsearch")

#load functions specific to bandsearch tasks####
source("bandsearch_functions.R")

#get the tables you need####
#rsfile <- read.dbf("resight21.dbf")%>%
#  mutate(STATUS=as.character(STATUS), STATUS=ifelse(STATUS=="BRB"|STATUS=="BR1","BR",STATUS))
bandinv <- read.dbf("../band/band_inv.dbf")
#rsfile <- rsfile[,1:25] #gets rid of all the "x" columns introduced by read.dbf
bandinv <- bandinv[, 1:7]
allresight <- read.dbf("allresight.dbf")
allresight <- allresight[, 1:26]
gdr_birds_df <-
  read.csv(
    "Z:/Informatics/S031/analyses/GDR/data/croz_royds_gdr_depl_all_v2021-08-27.csv"
  )

# table with breeding status
breed_stat <- 
  read_csv(r'(Z:\Informatics\S031\analyses\AdultSurvivorship\ResightAnalysis_2021\export_for_kate_090721.csv)',
           col_types = "cnccccccccccccccccccccccccccccccccnnc") %>%
  pivot_longer(cols = starts_with("rs"), 
               names_to = "season",
               values_to = "status") %>%
  mutate(stat_char = substr(status,3,3),
         bandnumb = as.numeric(bandnumb),
         seas_marked = as.numeric(seas),
         season_yr = substr(season,3,4)) %>%
  mutate(season_yr = as.numeric(ifelse(season_yr > 50, 
                                       paste0(19, season_yr),
                                       paste0(20, season_yr)))) %>%
  select(bandnumb,season_yr, br_status = stat_char)





#0000000000000000000000000000000000000000000000000000000000000000000000000
#number of birds seen in a given year that were seen in the prior year####
#0000000000000000000000000000000000000000000000000000000000000000000000000
#Filter allresight file to what you need it to be

allrs <- mutate(
  allresight,
  DATE = as.Date(DATE),
  season_yr = lapply(DATE, season_fullyr),
  BANDNUMB = as.numeric(BANDNUMB),
  season_yr = as.numeric(season_yr)
)

allrs$BANDSEEN[allrs$season_yr < 2002] <-
  "Y"#bandseen not introduced until 2002 - so all were seen

allrs <- filter(
  allrs,
  BANDNUMB > 0 & BANDNUMB < 99999 & season_yr < 2020 &
    season_yr > 1995 & BANDSEEN == "Y" &
    substring(STATUS, 1, 4) != "DEAD"
)

#now get rid of all records of individuals for which the band was ever reported LOST or REMOVED
#slct<-paste0("select * from allrs where bandnumb not in
#    (select distinct bandnumb from allrs where substring(STATUS,1,5)='REMOV'
#      or substring(STATUS,1,4)='LOST'
#      )")

slct <- paste0(
  "select * from allrs where bandnumb not in
    (select distinct bandnumb from allrs where substring(STATUS,1,5)='REMOV'
      or substring(STATUS,1,4)='LOST'
      )"
)

allrs <- sqldf(slct)
# rm_bands <-
# allrs_filt <- allrs%>%
#   filter(

#this removed 528 individuals; total here is 150309
#sort(unique(allrs$BANDSEEN))

#summary(allrs)
#summary(allrs$BANDNUMB)
#summary(allrs$season_yr)

#restrict to only birds banded as chicks (i.e. exclude BREF, WB)
slct <- paste0(
  "select allrs.*, bandinv.type from allrs, bandinv
     where allrs.bandnumb>=bandinv.LOW AND bandnumb<=bandinv.HIGH"
)
allrs_type <- sqldf(slct)
allrs <- filter(allrs_type, allrs_type$TYPE == "CHIC")
#137563 total here on 11/13/2021

#exclude birds which had GDR's attached:
slct <-
  "select allrs.* from allrs where bandnumb not in (select distinct bird_id from gdr_birds_df)"
allrs <- sqldf(slct)
#125889 now - note that all GDR birds get removed from the ENTIRE study


# now exclude pre-breeders
allrs_br <-
  allrs %>%
  left_join(breed_stat, 
            by = c("BANDNUMB" = "bandnumb",
                   "season_yr")) %>%
  filter(br_status != "P")



seas_list <- sort(unique(allrs$season_yr))
col_list <- c("BIRD", "CROZ", "ROYD")

#Make data frame for holding results####
resight_summary_df <-
  data.frame(
    season = integer(),
    colony = character(),
    seen_ty_and_ly = numeric(),
    seen_ly = numeric(),
    br_ly_seen_ty = numeric(),
    br_ly = numeric()
  )

for (col in col_list) {
  allrs_subset <- filter(allrs_br, COLONY == col)
  
  for (seas in seas_list) {
    seas <- as.character(seas)
    slct <-
      paste0(
        "select distinct bandnumb from allrs_subset where season_yr=",
        seas,
        " and bandnumb in (select bandnumb from allrs_subset where season_yr=",
        seas,
        "-1)"
      )#seen this year and last year
    seen <- sqldf(slct)
    slct2 <-
      paste0("select distinct bandnumb from allrs_subset where season_yr=",
             seas,
             "-1")#all birds seen last year
    seen2 <- sqldf(slct2)
    slct3 <-
      paste0(
        "select distinct bandnumb from allrs_subset where season_yr=",
        seas,
        " and bandnumb in (select distinct bandnumb from allrs_subset where season_yr=",
        seas,
        "-1 and ((nuegg>0 and nuegg<9) or (nuch>0 and nuch<9)))"
      )
    #seen this year, breeder last year
    seen3 <- sqldf(slct3)
    slct4 <-
      paste0(
        "select distinct bandnumb from allrs_subset where season_yr=",
        seas,
        "-1 and ((nuegg>0 and nuegg<9) or (nuch>0 and nuch<9))"
      ) #breeders last year
    seen4 <- sqldf(slct4)
    print(paste0("season: ", seas, " colony: ", col, " ", nrow(seen)))
    resight_summary_df[nrow(resight_summary_df) + 1, ] <-
      c(seas,
        col,
        nrow(seen),
        nrow(seen2),
        nrow(seen3),
        nrow(seen4))
  } #end of season loop
} #end of colony loop

#meake these columns into numbers
resight_summary_df <-
  mutate(resight_summary_df, season = as.numeric(season))
resight_summary_df <-
  mutate(resight_summary_df, seen_ty_and_ly = as.numeric(seen_ty_and_ly))
resight_summary_df <-
  mutate(resight_summary_df, seen_ly = as.numeric(seen_ly))
resight_summary_df <-
  mutate(resight_summary_df, br_ly_seen_ty = as.numeric(br_ly_seen_ty))
resight_summary_df <-
  mutate(resight_summary_df, br_ly = as.numeric(br_ly))

#add the proportions
resight_summary_df$prop_resight <-
  resight_summary_df$seen_ty_and_ly / resight_summary_df$seen_ly
resight_summary_df$prop_br_resight <-
  resight_summary_df$br_ly_seen_ty / resight_summary_df$br_ly
resight_summary_df$prop_resight <-
  as.numeric(sprintf(resight_summary_df$prop_resight, fmt = '%#.3f'))
resight_summary_df$prop_br_resight <-
  as.numeric(sprintf(resight_summary_df$prop_br_resight, fmt = '%#.3f'))

# write table

# write_csv(
#   resight_summary_df,
#   "Z:/Informatics/S031/analyses/gdr_molt_sic/data/resight_summary_no_PB.csv"
# )

