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

#load functions specific to bandsearch tasks####
source("code/bandsearch_functions.R")

#get the tables you need####
#rsfile <- read.dbf("resight21.dbf")%>%
#  mutate(STATUS=as.character(STATUS), STATUS=ifelse(STATUS=="BRB"|STATUS=="BR1","BR",STATUS))
bandinv <- read.dbf("data/band_inv.dbf")
#rsfile <- rsfile[,1:25] #gets rid of all the "x" columns introduced by read.dbf
bandinv <- bandinv[, 1:7]
allresight <- read.dbf("data/allresight.dbf")
allresight <- allresight[, 1:26]
gdr_birds_df <-
  read.csv(
    "data/croz_royds_gdr_depl_all_v2021-08-27.csv"
  )



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
slct <- paste0(
  "select * from allrs where bandnumb not in
    (select distinct bandnumb from allrs where substring(STATUS,1,5)='REMOV'
      or substring(STATUS,1,4)='LOST'
      )"
)

allrs <- sqldf(slct)

#restrict to only birds banded as chicks (i.e. exclude BREF, WB)
slct <- paste0(
  "select allrs.*, bandinv.type from allrs, bandinv
     where allrs.bandnumb>=bandinv.LOW AND bandnumb<=bandinv.HIGH"
)
allrs_type <- sqldf(slct)
allrs <- filter(allrs_type, allrs_type$TYPE == "CHIC")

#exclude birds which had GDR's attached:
slct <-
  "select allrs.* from allrs where bandnumb not in (select distinct bird_id from gdr_birds_df)"
allrs <- sqldf(slct)


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
  allrs_subset <- filter(allrs, COLONY == col)
  
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
#   "data/resight_summary_thru_2019.csv"
# )

