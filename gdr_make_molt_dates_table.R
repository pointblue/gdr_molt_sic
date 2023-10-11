# code to create table with dates of molt for each individual
# By A. Schmidt
# Re-run on 1/27/2021 to use revised divestats table
# Also changed molt period definition to rely on the number of foraging dives rather than the total number of dives

library(tidyverse)
library(data.table)
library(RcppRoll)
library(lubridate)



# read in all dive_stats ####
# this data table created from raw pressure data using code contained in https://github.com/pointblue/gdr_data_processing
dives <- fread("data/diveStats_ds_all_filt.csv")

# Read in deploy file
gdr_depl <- read_csv("data/croz_royds_gdr_depl_all_v2021-08-27.csv")%>%
  dplyr::mutate(start_rec=as.Date(start_rec, format="%m/%d/%Y"), end_rec = as.Date(end_rec, format="%m/%d/%Y"),
         end_jDay = as.numeric(format(end_rec,"%j")),
         deploy_id=seq(1:n()))


# calculate molt duration #####
# Date index
as.numeric(format(as.Date("2017-01-30"),"%j"))
# If not foraging before Feb 1 (jday32) = at colony, after Feb1=molting, after April 1 post-molt
col_d=28 # Jan 28
molt_d=105 # April 15
arr_d=274 # October 1


# define number of dives threshold for determining activity status
# if less than or equal to this number in a day, not diving
# tried 5, mean molt duration = 19.20197
# dive_tresh =10, mean molt duration = 19.46798
#  using 10 because it catches one bird who had a few dives recorded for much of "molt"
#  still missed one bird in 2018
#  switched to using the number of foraging dives and set threshold to 0

# molt criteria tested
# 7 day mean of number of dives per day = zero: 25 birds with no molt dates
# 7 day mean of number of dives >5m = 0: 13 birds with no molt dates, 3 or 4 that possible should, some suspiciously short molts
# 7 day mean number of dives >5m <5: 9 birds with no molt dates, 1 or 2 that maybe should (71865, 711001369), one very short (70449)
    # 711001369 looks like it died before the end of molt so no molt end for that bird
# dive threshold of 10: 10 birds with no molt dates, 2 that should have them (70449, 71865 from 2018). One additional (46645 from 2018) has a very short estimated molt period. Mean molt dur = 19.06
# dive thresh=15: same birds dont have molt dates but 46645 now has more reasonable molt period. Mean molt duration = 19.28




# calculated the number of dives each day
# Group by individual and by date
dive_tab<- dives%>%
  mutate(date=as.Date(date))%>%
  # filter(maxdep>5)%>%
  # # join in deploy data to add deploy and retrieved dates
  left_join(dplyr::select(gdr_depl,bird_id,deploy_date,retrieve_date,end_rec,season,br_col))%>%
  group_by(bird_id,date,season,deploy_date,retrieve_date,end_rec)%>%
  # Summarize # of dives by individual and day
  summarise(n_dives=n(),mean_dur=mean(divetim),divetim = sum(divetim),.groups="drop")%>%
  # remove deploy_date and retrieve_date as grouping variables before filling in missing days
  group_by(bird_id,season)%>%
  # fill in rows for days between deploy and retrieve dates with no dives
  complete(date=seq(first(deploy_date), min(first(retrieve_date),first(end_rec)), by="day"), fill=list(n_dives=0))%>%
  # deploy date and retrieve date disappearing for some reason join with gdr_depl again to get back
  dplyr::select(-deploy_date,-retrieve_date, -end_rec)%>%
  left_join(dplyr::select(gdr_depl,deploy_id,bird_id,season, deploy_date,retrieve_date,end_rec),by=c("bird_id", "season"))%>%
# need to remove the extra rows for the bird that was retrieved 2 yr later
  mutate(n_dives=ifelse(bird_id==63137&season==2016&date>as.Date("2017-11-19"),NA,n_dives))%>%
  # fill in divetim NAs with 0
  mutate(divetim=ifelse(is.na(divetim),0,divetim))

# manually determine molt dates for 70449 and 71865 in 2018 ####
b70449 <- dive_tab%>%
  filter(bird_id==70449,season==2018)
# molt start = as.Date(2019-02-13)
# molt end = 2019-03-04

b71865 <- dive_tab%>%
  filter(bird_id==71865,season==2018)
# molt start = 2019-02-23
# molt end = 2019-03-13

# also checking 46645 because it was also somewhat noisy
b46645 <- dive_tab%>%
  filter(bird_id==46645,season==2018)
# start molt 2019-02-11 (same as auto method below)
# end molt 2019-03-05 (two days later than determined below)

dive_thresh = 15
# add column and fill in with status based on date and diving behavior
status_tab <- dive_tab%>%
  # add column for jDay
  mutate(jDay=as.numeric(format(date, "%j")),
         # add status based on number of dives per day
         # If averaging 15 or fewer dives in a day after Jan 28 and before April 15,and for the 7 days before or after, status = molt
         status = ifelse((jDay > col_d &
                              jDay < molt_d &
                              ((n_dives <= dive_thresh & RcppRoll::roll_mean(n_dives,7,fill=NA,align="left")<=dive_thresh)|# average of next 7 days <= dive threshold
                              (n_dives<=dive_thresh & RcppRoll::roll_mean(n_dives,7,fill=NA,align="right")<=dive_thresh))), # average of previous 7 days <= dive threshold
                         "molt",        
                         NA),
        # # If not diving for only one day, before Feb 1, at colony
        status=ifelse(is.na(status)&((jDay<col_d|jDay>arr_d)&n_dives<=dive_thresh),"at_col",status),
        # #fill in if arrived
        # if after October first of the year after deployment and averageing fewer dives than threshold for 5 days, then mark as arrived
        # or if date = date_retrieved
        status=ifelse(date>(deploy_date+days(300))&jDay>arr_d&(n_dives<=dive_thresh&RcppRoll::roll_mean(n_dives,5,fill=NA,align="left")<=dive_thresh)|date==retrieve_date,"arrived",status))

# list of bird ids that should have molt dates
bird_ids <- status_tab%>%
  group_by(bird_id,season)%>%
  slice_head()%>%
  dplyr::select(-n_dives,-mean_dur,-divetim,-jDay,-status,-date)


molt_tab <-status_tab%>%
  # fill in molt dates for birds that should them (see above)
  mutate(
    status = case_when(
      bird_id==70449&season==2018&date==as.Date("2019-02-13")|
        bird_id==71865&season==2018&date==as.Date("2019-02-23")~"molt",
      bird_id==70449&season==2018&date==as.Date("2019-03-04")|
      bird_id==71865&season==2018&date==as.Date("2019-03-13")~"molt",
      bird_id==46645&season==2018&date==as.Date("2019-03-05")~"molt",
      TRUE~status))%>%
  # Re-group and rearrange for joining
  group_by(bird_id,season,status)%>%
  # slice to retain first and last date of status
  slice(c(1,n()))%>%
  arrange(bird_id,date)%>%
  filter(status=="molt")%>%
  mutate(status=ifelse(row_number()==1,paste0("start_",status),paste0("end_",status)))%>%
  ungroup()%>%
  dplyr::select(-n_dives,-mean_dur,-divetim)%>%
  filter(status%in%c("start_molt","end_molt"))%>%
  #join colony info
  left_join(dplyr::select(gdr_depl,bird_id,bird_fn,season,br_col))%>%
  dplyr::select(-jDay)%>%
  spread(key=status,value=date)%>%
  mutate(start_jDay = as.numeric(format(start_molt,"%j")), end_jDay =as.numeric(format(end_molt,"%j")),
         molt_dur=end_jDay-start_jDay+1)%>%
  # filter out birds who's tags died before the end of molt or did not have molt dates
  filter(end_molt<end_rec)

# check birds that did not have molt dates identified that should have or had short molts
# only one bird, 711001369 in 2018 had a device die during molt
anti_join(bird_ids,molt_tab)
# all birds missing were devices that died before molt
  
# mean molt duration
mean(molt_tab$molt_dur) # 19.2981

# check figure to see if molt dates making sense
p2016 <-
  molt_tab%>%
  filter(season==2016)%>%
  mutate(bird_id = fct_reorder2(factor(bird_id), season,start_molt))%>%
  ggplot() +
  geom_segment(aes(x=start_molt, xend=end_molt, y=factor(bird_id), yend=factor(bird_id), col=br_col), size=3)+
  # facet_wrap(~season,scales="free")+
  xlab("molt period")+
  ylab("Bird ID")+
  theme(axis.text = element_text(size =6),legend.position="none")#axis.text.y = element_text(angle=90))
p2016

p2017 <- 
  molt_tab%>%
  filter(season==2017)%>%
  mutate(bird_id = fct_reorder2(factor(bird_id), season,start_molt))%>%
  ggplot() +
  geom_segment(aes(x=start_molt, xend=end_molt, y=factor(bird_id), yend=factor(bird_id), col=br_col), size=3)+
  # facet_wrap(~season,scales="free")+
  xlab("molt period")+
  ylab("Bird ID")+
  theme(axis.text = element_text(size =6), legend.position = "none")#axis.text.y = element_text(angle=90))

p2018 <- 
  molt_tab%>%
  filter(season==2018)%>%
  mutate(bird_id = fct_reorder2(factor(bird_id), season,start_molt))%>%
  ggplot() +
  geom_segment(aes(x=start_molt, xend=end_molt, y=factor(bird_id), yend=factor(bird_id), col=br_col), size=3)+
  # facet_wrap(~season,scales="free")+
  xlab("molt period")+
  ylab("Bird ID")+
  theme(axis.text = element_text(size =6),legend.position = "top")+#axis.text.y = element_text(angle=90))
  guides(color=guide_legend(title="Colony"))

gridExtra::grid.arrange(p2016,p2017,p2018,ncol=3)


dates <- molt_tab%>%
  dplyr::select(br_col,bird_id,bird_fn,season,deploy_date,retrieve_date,start_molt, end_molt, molt_dur)

write_csv(dates,paste0("data/gdr_molt_dates_v",Sys.Date(),".csv"))


