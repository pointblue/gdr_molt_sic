# Create table with molt locations from 500 chains ####
# A. Schmidt

library(tidyverse)


# Map using 500 chains
# read in gdr deploy file
gdr_depl <- read_csv("data/croz_royds_gdr_depl_all_v2021-08-27.csv")
# read in table with last 500 chains
locs500 <- read_csv("data/gdr_locs_final_500_all_yr_v2021-06-03.csv")
# Table with molt and arrival dates estimated from GDR data
molt_tab<- read_csv("data/gdr_molt_dates_v2022-02-11.csv")


# join molt dates to gdr_depl
gdr_molt <-gdr_depl%>%
  left_join(molt_tab)%>%
  filter(!is.na(molt_dur))%>%
  # add molt DOY column
  mutate(mstart_doy=as.numeric(format(start_molt, "%j")),
         mend_doy = as.numeric(format(end_molt, "%j")))%>%
  mutate(breed_cat = factor(ifelse(breeder == 0,1,ifelse(breeder==1&success==0,2,3))),
         age_cat=factor(ifelse(age<5,1,ifelse(age>4&age<12,2,3))),
         arr_doy = as.numeric(format(arrived,"%j")),
         breed_next_cat = factor(ifelse(breeder_next == 0,0,ifelse(breeder_next==1&success_next==0,2,3))))
#join individual data to locations and filter to days between start and end of molt
molt_locs <-locs500%>%
  mutate(doy=as.numeric(format(time1,"%j")))%>%
  left_join(dplyr::select(gdr_molt,season,bird_id,bird_fn, start_molt,end_molt,mstart_doy,mend_doy,br_col))%>%
  filter(doy>=mstart_doy&doy<=mend_doy,inserted==FALSE)%>%
  mutate(year=factor(season+1))


# molt_locs <- read_csv("../data/molt_locs500.csv")

# a few checks
molt_locs%>%
  group_by(season,bird_id)%>%
  tally()%>%
  group_by(season)%>%
  tally()
# 65 birds from 2016
# 76 birds from 2017
# 54 birds from 2018 with molt locations
# 195 total


mdays <- molt_locs%>%
  group_by(season,bird_id,doy)%>%
  tally()%>%
  group_by(season, bird_id)%>%
  tally()
range(mdays$n)
median(mdays$n)
hist(mdays$n)
# 1-26 days of locations for each bird during molt
# median of 17 days of locations

# check birds that have fewer than 5 days of molting locations
no_mlocs <- mdays%>%
  filter(n<5)%>%
  left_join(select(molt_tab, bird_id,start_molt,end_molt))%>%
  mutate(end_doy=as.numeric(format(end_molt, "%j")))
summary(no_mlocs$end_doy)

few_locs <- molt_locs%>%
  group_by(season,bird_id)%>%
  tally()%>%
  filter(n<2500)%>%
  left_join(select(molt_tab,season, bird_id,start_molt,end_molt))%>%
  mutate(end_doy=as.numeric(format(end_molt, "%j")))# 15 birds with fewer than 5 (2500) locations estimated during molt
summary(few_locs$end_doy)
write_csv(few_locs, "data/few_molt_locs.csv")

mlocs <- mdays%>%
  filter(n>5)%>%
  left_join(select(gdr_molt, bird_id,start_molt,end_molt,))
summary(mlocs$end_molt)


write_csv(molt_locs, paste0("data/molt_locs500_",Sys.Date(),".csv"))
