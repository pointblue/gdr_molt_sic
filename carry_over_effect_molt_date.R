# Carry over effects and molt
# 
# 
# To Add:
# location at end of molt
# Is longitude better estimated than latitude at that time of year?
# 
# Figures
#    summarising molt dates
#     molt duration
#     molt location
#     typical dive pattern before and after molt
#     figure showing distribution of molt dates with molt locations (to show how biased sample is)
#     

library(tidyverse)
library(lubridate)
library(data.table)

# GDR deploy file
gdr_depl<- read_csv("Z:/Informatics/S031/analyses/GDR/data/croz_royds_gdr_depl_all_v2021-08-27.csv")

# Table with molt and arrival dates estimated from GDR data
molt_tab<- read_csv("Z:/Informatics/S031/analyses/GDR/data/gdr_molt_arrival_dates_v2021-09-09.csv")

# join molt dates to gdr_depl
gdr_molt <-gdr_depl%>%
  left_join(molt_tab)%>%
  filter(!is.na(molt_dur))%>%
  # filter(sex!="U",study=="KA")%>% # think I don't want to actually filter out the U's because they are mostly from Royds
  # add molt DOY column
  mutate(mstart_doy=as.numeric(format(start_molt, "%j")),
         mend_doy = as.numeric(format(end_molt, "%j")))%>%
  mutate(breed_cat = factor(ifelse(breeder == 0,0,ifelse(breeder==1&success==0,2,3))),
         age_cat=factor(ifelse(age<5,1,ifelse(age>4&age<12,2,3))),
         arr_doy = as.numeric(format(arrived,"%j")),
         breed_next_cat = factor(ifelse(breeder_next == 0,0,ifelse(breeder_next==1&success_next==0,2,3))))
# 71865 doesn't have a molt date so gdr_molt 1 less row than molt_tab

# dive data

dive <- fread("Z:/Informatics/S031/analyses/GDR/data/diveStats_all.csv")

# summarise to number of Foraging dives per day
 fdives <- dive%>%
  filter(divetype_ds=="F")%>%
  group_by(bird_id,season,date)%>%
  tally()%>%
  # join molt start date so can use to calculate the mean number of foraging dives per day in the 2-weeks prior to molt start
  left_join(dplyr::select(gdr_molt,bird_id,season,start_molt))%>%
  # filter out dives from birds with no molt date
  filter(!is.na(start_molt))
 
 d_dat<-dive%>%
   filter(bird_id==49990)%>%
   group_by(bird_id,date)%>%
   tally()%>%
   dplyr::select(bird_id,date,n)


# 
# Color palettes####
# # # Color for Crozier
 col1="#006C84" 
 col2="#B2DBD5" # arctic
 col3="#5EA8A7" # lagoon
 
 # color palette
 col.p <- c("#006C84","#5EA8A7","#B2DBD5","white")
 # turquoise to orange
 col.to <- c("#006C84","#B2DBD5","#e6ceb5","#ff8324","#f73c2f")
 
 # pb 2 color palette,
 col.pb <- c("#BFD730","#F7941D","#666666","#A7A9AC")

 pb_col1 <- "#005BAA"
 pb_col2 <- "#4495D1"


 
 # custom theme
 wsc_theme <- function(){
   theme_classic() %+replace%
   theme(
     axis.title.y=element_text(size=16,margin = margin(r = 15), angle=90),
     axis.title.x=element_text(size=16,margin = margin(t = 15, r = 0, b = 0, l = 0)),
     axis.text=element_text(size=12),
     legend.text = element_text(size=12),
     legend.title = element_text(size=16)
   )
 }
 
 # plot of example bird with foraging dives per day #### 
 
 ggplot(d_dat,aes(date,n))+
   geom_bar(stat="identity", col=pb_col2,fill=pb_col2)+
   # xlim(as.Date("2017-01-30"),as.Date("2017-04-30"))+
   ylab("Number of Dives per Day")+
   xlab("Date")+
   scale_x_continuous(breaks = c(as.Date("2017-01-01"),as.Date("2017-03-01"),as.Date("2017-05-01"),as.Date("2017-07-01"),as.Date("2017-09-01"),as.Date("2017-11-01")),
                      labels=c("Jan 1","Mar 1","May 1","Jul 1", "Sept 1","Nov 1"))+
   wsc_theme()
 
 
# zoom in
ggplot(d_dat,aes(date,n))+
  geom_bar(stat="identity", col=pb_col2,fill=pb_col2)+
  # xlim(as.Date("2017-01-30"),as.Date("2017-04-30"))+
  ylab("Number of Dives per Day")+
  xlab("Date")+
  scale_x_continuous(limits =c(as.Date("2017-01-30"),as.Date("2017-04-30")),  breaks = seq(as.Date("2017-01-30"),as.Date("2017-04-30"),by=10),
                     labels=c("Jan 30","Feb 9","Feb 19","Mar 1", "Mar 11","Mar 21","Mar 31","Apr 10","Apr 20","Apr 30"))+
  wsc_theme()
  
 
 # fdives_day <- dive%>%
#   mutate(month=month(date),doy=as.numeric(format(date,"%j")))%>%
#   filter(divetype_ds=="F",month%in%c(1:3))%>%
#   group_by(bird_id,season,doy)%>%
#     tally()%>%
#   group_by(season,doy)%>%
#   summarise(mean=mean(n))



# fdives_summ <- fdives%>%
#   # calculate average foraging dives per day in week and 2 week prior to molt start
#   mutate(#fdive_7day=ifelse((start_molt-date)<=7&(start_molt-date)>0,1,0))%>%
#          fdive_14day=ifelse((start_molt-date)<=14&(start_molt-date)>0,1,0))%>%
#   group_by(bird_id,season,fdive_14day)%>%
#   summarise(fdive_mean=mean(n,na.rm=TRUE))%>%
#   filter(fdive_14day==1)
# 
# hist(fdives_summ$fdive_mean)


# Median locations from GDRs ####
locs <- read_csv("Z:/Informatics/S031/analyses/GDR/data/gdr_locs_all.csv")

# # location on March 15
# mar15_locs <- locs%>%
#   mutate(doy=as.numeric(format(date,"%j")))%>%
#   left_join(select(gdr_depl,season,bird_id,bird_fn))%>%
#   filter(doy==75,inserted==FALSE)%>%
#   mutate(season=factor(season))%>%
#   select(lon50_m15=lon50,lat50_m15=lat50)

molt_locs <-locs%>%
  mutate(doy=as.numeric(format(date,"%j")))%>%
  left_join(dplyr::select(gdr_molt,season,bird_id,bird_fn, start_molt,end_molt,mstart_doy,mend_doy))%>%
  filter(doy==mstart_doy|doy==mend_doy,inserted==FALSE)%>%
  mutate(season=factor(season))

mstart_locs <- molt_locs%>%
  filter(doy==mstart_doy)

mend_locs <-molt_locs%>%
  filter(doy==mend_doy)%>%
  mutate(lon50_mend=lon50,lat50_mend=lat50,locID=row_number())

# write table for other uses
write_csv(mend_locs,"Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/molt_end_locs.csv")

# calculate difference between Lat and lon 2.5 and 97.5

library(raster)
library(sf)
library(rgdal)
# have to project and re-project so that can calculate distance in meters
proj_ant<-CRS("+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=180 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
mend_2.5_sf <- st_as_sf(mend_locs,coords = c("lon2.5","lat2.5"),
                                        crs = "+proj=longlat +ellps=WGS84 +lon_0=-169 +lon_wrap")
mend_2.5_t <- ggspatial::df_spatial(st_transform(mend_2.5_sf,proj_ant))%>%
  rename(lon2.5=x,lat2.5=y)%>%
  dplyr::select(lon2.5,lat2.5,locID)

mend_97.5_sf <- st_as_sf(mend_locs,coords = c("lon97.5","lat97.5"),
                        crs = "+proj=longlat +ellps=WGS84 +lon_0=-169 +lon_wrap")
mend_97.5_t <- ggspatial::df_spatial(st_transform(mend_97.5_sf,proj_ant))%>%
  mutate(lon97.5=x,lat97.5=y)%>%
  dplyr::select(lon97.5,lat97.5,locID)

mend_locs_CI <-full_join(mend_2.5_t,mend_97.5_t)%>%
  mutate(lat_dif=abs(lat97.5-lat2.5)/1000,lon_dif = abs(lon97.5-lon2.5)/1000,lat_lon_dif=(lon_dif-lat_dif))

mend_locs_CI%>%
  pivot_longer(cols=c("lat_dif","lon_dif"),names_to="coord",values_to="dif")%>%
  ggplot(aes(dif))+
  geom_histogram(aes(fill=coord),alpha=0.7,position="identity", binwidth = 20)+
  xlab("Difference between 2.5 and 97.5 position at end of molt (km)")

lat_exclude<-as.numeric(quantile(mend_locs_CI$lat_dif)[4])
lon_exclude<-as.numeric(quantile(mend_locs_CI$lon_dif)[4])


mend_locs_conf <- mend_locs%>%
  left_join(dplyr::select(mend_locs_CI,lat_dif,lon_dif,locID))%>%
  # flag for excluding
  mutate(exclude=ifelse((lat_dif>!!lat_exclude)|(lon_dif>!!lon_exclude),1,0))# this labels 149 locations to remove


mend_locs_conf%>%
  group_by(exclude)%>%
  tally() # this labels 149 locations to remove


mend_locs_filt <- mend_locs_conf%>%
  filter(exclude==0)%>%
  group_by(bird_fn)%>%
  slice(1)
  dplyr::select(bird_id,bird_fn,lat50_mend,lon50_mend)

# join back to gdr_molt

gdr_molt_locs <-gdr_molt%>%
  left_join(mend_locs_filt, by="bird_fn")

# write table for other uses
# write_csv(gdr_molt,"Z:/Informatics/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/molt_end_locs.csv")


# 71865 does not have a molt period identified (never had an extended period with <5 dives per day)
# 
# Crozier nest and chick count data
c_nest_count <- read.csv("Z:/Informatics/S031/S0312021/EOS_report/data/croz_adult_count_compiled_v2021-04-08.csv")%>%
  mutate(subcol=factor(subcol))%>%
  dplyr::select(-date)

c_chick_count <- read.csv("Z:/Informatics/S031/S0312021/EOS_report/data/croz_chick_count_compiled_v2021-04-08.csv")%>%
  mutate(subcol=factor(subcol))%>%
  dplyr::select(-date)
  
c_prod <- c_nest_count%>%
  left_join(c_chick_count)%>%
  mutate(prod=ch_ct/active_ct)%>%
  group_by(season)%>%
    summarise(mean_prod=mean(prod,na.rm=TRUE))%>%
  mutate(br_col="CROZ")%>%
  filter(season>2015&season<2020)

# Royds nest and chick count data
r_prod <- read.csv("Z:/Informatics/S031/S0312021/EOS_report/data/royd_all_count_compiled_v2021-04-09.csv")%>%
  filter(active_ct>0)%>%
  mutate(prod=ch_ct/active_ct,season=season_fy)%>%
  group_by(season)%>%
  summarise(mean_prod=mean(prod,na.rm=TRUE))%>%
  mutate(br_col="ROYD")%>%
  filter(season>2015&season<2020)

prod <- bind_rows(c_prod,r_prod)

# Join all data of interest to make data table for models
molt_dat<-gdr_molt%>%
  # add mean fdives in 7days prior to molt start
  # left_join(fdives_summ)%>%
  # add bqi category
  mutate(bqi_cat = factor(ifelse(bqi<quantile(gdr_molt$bqi, na.rm=TRUE)[2],1,
                                 ifelse(bqi>=quantile(gdr_molt$bqi, na.rm=TRUE)[2]&
                                          bqi<quantile(gdr_molt$bqi, na.rm=TRUE)[4],2,3))))%>%
# add mean productivity by year
  left_join(prod)%>%
  dplyr::select(study,start_molt,end_molt,mstart_doy,br_col,age,season,sex,bqi,bqi_cat,mean_prod,breed_cat,bird_id,
         molt_dur,arr_doy,breeder_next,success_next,mend_doy)%>%
  filter(study=="KA",sex!="U")%>%
  drop_na%>%
  mutate(year=factor(season+1),br_col=factor(br_col))
  # mutate_at(vars(age,bqi),list(scale))



# summary plots of molt start date ####

molt_dat%>%
  ggplot(aes(x=br_col,y=mstart_doy, fill=year))+
  geom_boxplot(col="grey50")+
  # seabiRd::scale_fill_seabird(palette="chileanskua",discrete = TRUE, name = "Year")+
  # scale_fill_gradientn(colors=col.p,discrete)+
  scale_fill_manual(values=col.pb[c(1,2,4)],name="Year")+
  wsc_theme()+
  xlab("Breeding Colony")+
  ylab("Molt Start Date")+
  scale_y_continuous(breaks=seq(30,80,by=10),labels=c("Jan 30","Feb 9","Feb 19","Mar 1", "Mar 11","Mar 21"))
  

# plot of molt duration ####
molt_dat%>%
  ggplot(aes(x=br_col,y=molt_dur, fill=year))+
  geom_boxplot(col="grey50")+
  # seabiRd::scale_fill_seabird(palette="chileanskua",discrete = TRUE, name = "Year")+
  # scale_fill_gradientn(colors=col.p,discrete)+
  scale_fill_manual(values=col.pb[c(1,2,4)],name="Year")+
  wsc_theme()+
  xlab("Breeding Colony")+
  ylab("Molt Duration (days)")
  scale_y_continuous(breaks=seq(30,80,by=10),labels=c("Jan 30","Feb 9","Feb 19","Mar 1", "Mar 11","Mar 21"))


  
mean(molt_dat$molt_dur)
range(molt_dat$molt_dur)

mean(molt_dat$mstart_doy)
range(molt_dat$mstart_doy)

# calculate range of dates that includes ~95% of individuals for each year
# 
# Calculate early cutoff for each season
early_cutoff <- molt_dat %>%
  group_by(season)%>%
  summarize(molt_start = quantile(mstart_doy,c(0.025)),quant = c("early_cut"))%>%
  tidyr::pivot_wider(id_cols = season,names_from = quant, values_from=molt_start)

# join to molt dates table and filter individuals that started molting earlier than the cutoff
mstart_filt <- molt_dat%>%
  left_join(early_cutoff)%>%
  filter(mstart_doy>=early_cut)

# start with all data again to calculate 97.5% cut off for end of molt
late_cutoff<- molt_dat%>%
  group_by(season)%>%
  summarize(molt_end = quantile(mend_doy,c(0.975)),quant = c("late_cut"))%>%
  tidyr::pivot_wider(id_cols = season,names_from = quant, values_from=molt_end)

mend_filt <- mstart_filt%>%
  left_join(late_cutoff)%>%
  left_join(early_cutoff)%>%
  filter(mstart_doy>=early_cut&mend_doy<=late_cut)%>%
  group_by(season)%>%
  summarise(start_molt=min(mstart_doy), end_molt = max(mend_doy))

ggplot(molt_dat,aes(mstart_doy))+
  geom_histogram()+
  facet_wrap(~season)
  
mend_filt%>%
  group_by(season)%>%
  tally()

molt_dat%>%
  group_by(season)%>%
  tally()

write.csv(cutoff,"molt_dates_by_season.csv")
    


# Model molt start date ####
library(lme4)
library(nlme)
library(sjstats)
library(MuMIn)
library(jtools)
library(performance)

options(na.action = "na.fail") 
global_model <-lmer(mstart_doy~br_col+(age+I(age^2))*year+sex*year+breed_cat*year+bqi*year+bqi*breed_cat
                    # +(age+I(age^2))*mean_prod+sex+breed_cat*mean_prod
                    +(1|bird_id), REML=FALSE, data=molt_dat)
performance(global_model)


combinations1 <- dredge(global_model,extra = c("R^2", F = function(x)
  summary(x)$fstatistic[[1]]),REML=FALSE)

head(combinations1)

top <- lmer(mstart_doy~age+breed_cat+year+(1|bird_id),REML=FALSE, data=molt_dat)
summary(top)
performance(top)
sjPlot:: tab_model(top)

# # model averaging
# model.avg(combinations1, subset = delta < 4)

par(mar = c(3,5,6,4))
plot(combinations1,labAsExpr=TRUE)


# Effects plot for top molt date model####
effects::plot(top)

top_effects<-as.data.frame(effects::Effect("age", partial.residuals=T, top))
plot(top_effects)


LMERConvenienceFunctions::plotLMER.fnc(top) 
ggplot(top_effects,aes(age,fit))+
  geom_line()


jtools::plot_summs(top,scale=TRUE)

effect_plot(top,pred=age, interval=TRUE,int.type = "confidence", data=molt_dat,scale=FALSE, xlab="Molt Start Day")
jtools::effect_plot(top,pred=season, interval=TRUE,int.type = "confidence", data=molt_dat)
jtools::effect_plot(top,pred=br_col, interval=TRUE,int.type = "confidence", data=molt_dat)

interactions::interact_plot(top,pred=age,modx=season, data=molt_dat, interval=TRUE,int.type="confidence")
interactions::cat_plot(top,pred=season,modx=sex, data=molt_dat, interval=TRUE,int.type="confidence")
interactions::interact_plot(top,pred=bqi,modx=season, data=molt_dat, interval=TRUE,int.type="confidence")


plot(top) # model residuals look good



# plot model effects ggplot####
pm_dat <- as.data.frame(effects::effect(term="age",mod=top))
p.age <- ggplot() + 
  geom_line(data=pm_dat,aes(x=age, y=fit), color=col.pb[1], size=1)+
  #5
  geom_ribbon(data=pm_dat,aes(x=age, ymin=lower, ymax=upper), alpha= 0.3, fill=col.pb[1]) +
  # geom_rug(data=molt_dat,aes(age,mstart_doy),length = unit(0.005, "npc"))+
  #6
  labs(x="Age", y="Molt Start Date")+
  wsc_theme()+
  scale_y_continuous(lim=c(42,65),breaks=seq(40,65,by=5),labels=c("Feb 9","Feb 14","Feb 19","Feb 24","Mar 1","Mar 6"))

plot(p.age)



# Effect of breeding status
# 
pm_dat2 <- as.data.frame(effects::effect(term="breed_cat",mod=top))
p.breed <-ggplot() + 
  geom_point(data=pm_dat2,aes(x=breed_cat, y=fit), color=col.pb[2], size=3)+
  #5
  geom_errorbar(data=pm_dat2,aes(x=breed_cat, ymin=lower, ymax=upper), col=col.pb[2],width=0.1,size=1)+
  # geom_rug(data=molt_dat,aes(age,mstart_doy),length = unit(0.005, "npc"))+
  #6
  labs(x="Breeding Status", y="")+
  wsc_theme()+
  scale_y_continuous(lim=c(42,65),breaks=seq(40,65,by=5),labels=c("Feb 9","Feb 14","Feb 19","Feb 24","Mar 1","Mar 6"))+
  scale_x_discrete(labels=c("Non-Breeder","Failed Breeder" ,"Successful Breeder"))

plot(p.breed)


# Effect of year
pm_dat3 <- as.data.frame(effects::effect(term="year",mod=top))
# 
p.year <-ggplot() + 
  geom_point(data=pm_dat3,aes(x=year, y=fit), color=col.pb[3], size=3)+
  #5
  geom_errorbar(data=pm_dat3,aes(x=year, ymin=lower, ymax=upper), col=col.pb[3],width=0.1,size=1)+
  # geom_rug(data=molt_dat,aes(age,mstart_doy),length = unit(0.005, "npc"))+
  #6
  labs(x="Year", y="")+
  wsc_theme()+
  scale_y_continuous(lim=c(42,65),breaks=seq(40,65,by=5),labels=c("Feb 9","Feb 14","Feb 19","Feb 24","Mar 1","Mar 6"))
  # scale_x_discrete(labels=c("Non-Breeder","Failed Breeder" ,"Successful Breeder"))

plot(p.year)

gridExtra::grid.arrange(p.age,p.breed,p.year,ncol=3,nrow=1)

# check correlation between BQI and breeding status
ggplot(molt_dat,aes(bqi,as.numeric(breed_cat)))+
  geom_point()+
  geom_smooth(method="lm")

cor.test(molt_dat$bqi,as.numeric(molt_dat$breed_cat)) #r = -0.19, p<0.05


# modeling molt location ####
# 
# make appropriate table
#
#
molt_loc_dat<-gdr_molt%>%
  mutate(year=factor(season+1),season=factor(season),br_col=factor(br_col),sex=factor(sex))%>%
  filter(sex!="U")%>%
  left_join(mend_locs_conf)%>%
  filter(exclude==0)%>%
  dplyr::select(study,mstart_doy,br_col,age,season,sex,bqi,breed_cat,bird_id,
                         molt_dur,arr_doy,breeder_next,success_next,mend_doy,mstart_doy,lat50_mend,lon50_mend)%>%
  # mutate_at(vars(age,bqi,mend_doy,mstart_doy),list(scale))%>%
  drop_na()%>%
  # select only one location per day
  group_by(season,bird_id)%>%
  slice(1)%>%
  ungroup()

# locs500 <- read_csv("Z:/Informatics/S031/analyses/GDR/data/gdr_locs_final_500_all_yr_v2021-06-03.csv")

# molt_loc_dat <-locs500%>%
  # mutate(doy=as.numeric(format(time1,"%j")))%>%
  # left_join(dplyr::select(gdr_molt,season,bird_id,bird_fn, start_molt,end_molt,mstart_doy,mend_doy,br_col))%>%
  # filter(doy>=mstart_doy&doy<=mend_doy,inserted==FALSE)%>%
  # mutate(year=factor(season+1))


m_lon <- lm(lon50_mend~mstart_doy*breed_cat+mstart_doy*season+sex+br_col+age,data=molt_loc_dat)


hist(molt_loc_da$lat50_mend)
summary(m_lon)
performance::r2(m_lon)

lon_comb <-dredge(m_lon,REML=FALSE)
head(lon_comb)

lon_top <- lm(lon50_mend~br_col,data=molt_loc_dat)
summary(lon_top)


# plot of top longitude model####
plon_dat <- as.data.frame(effects::effect(term="br_col",mod=lon_top))
p.lon 
  
  ggplot() + 
  geom_point(data=plon_dat,aes(x=br_col, y=fit), color=col.pb[1], size=3)+
  #5
  geom_errorbar(data=plon_dat,aes(x=br_col, ymin=lower, ymax=upper), col=col.pb[1],width=0.1,size=1)+
  # geom_rug(data=molt_dat,aes(age,mstart_doy),length = unit(0.005, "npc"))+
  #6
  labs(x="Breeding Colony", y="Longitude")+
  wsc_theme()
  # scale_y_continuous(lim=c(42,65),breaks=seq(40,65,by=5),labels=c("Feb 9","Feb 14","Feb 19","Feb 24","Mar 1","Mar 6"))+
  # scale_x_discrete(labels=c("Non-Breeder","Failed Breeder" ,"Successful Breeder"))





jtools::plot_summs(lon_top,scale=TRUE)
jtools::effect_plot(lon_top,pred=br_col, interval=TRUE,int.type = "confidence", data=molt_loc_dat)


m_lat <- lm(lat50_mend~mstart_doy*breed_cat+mstart_doy*season+sex+br_col+age,data=molt_loc_dat)
summary(m_lat)

lat_comb <-dredge(m_lat,REML=FALSE)
head(lat_comb)
lat_comb

lat_top <- lm(lat50_mend~mstart_doy,data=molt_loc_dat)
summary(lat_top)


cor.test(molt_loc_dat$mstart_doy,molt_loc_dat$lat50_mend)

ggplot(molt_loc_dat,aes(mstart_doy,lat50_mend))+
  geom_point()+
  geom_smooth(method="lm")

jtools::plot_summs(m_lon,scale=TRUE)
jtools::effect_plot(m_loc,pred=mstart_doy,plot.points=TRUE, interval=TRUE,int.type = "confidence", data=molt_dat)


cor.test(molt_dat$mstart_doy,molt_dat$molt_dur)
ggplot(molt_dat,aes(mstart_doy,molt_dur))+
  geom_point(aes(mstart_doy,molt_dur))+
  geom_smooth(method="lm")

# plotting latitude model ####

plat_dat <- as.data.frame(effects::effect(term="mstart_doy",mod=lat_top))
p.lat 

ggplot() + 
  geom_line(data=plat_dat,aes(x=mstart_doy, y=fit), color=col.pb[2], size=1)+
  #5
  geom_ribbon(data=plat_dat,aes(x=mstart_doy, ymin=lower, ymax=upper), alpha= 0.3, fill=col.pb[2]) +
  # geom_rug(data=molt_dat,aes(age,mstart_doy),length = unit(0.005, "npc"))+
  #6
  labs(x="Molt Start Date", y="Latitude")+
  wsc_theme()+
  scale_x_continuous(lim=c(28,62),breaks=seq(30,60,by=5),labels=c("Jan 30","Feb 4","Feb 9","Feb 14","Feb 19","Feb 24","Mar 1"))
# ntinuous(lim=c(42,65),breaks=seq(40,65,by=5),labels=c("Feb 9","Feb 14","Feb 19","Feb 24","Mar 1","Mar 6"))+
# scale_x_




# map of molt locations

cor.test(gdr_molt$mstart_doy,as.numeric(format(gdr_molt$arrived, "%j")))
cor.test(gdr_molt$bqi,gdr_molt$fdive_mean)

m2 <- glm(breeder_next~mstart_doy+molt_dur,data=gdr_molt,family="binomial")
performance::r2(m2)

m3 <- glm(success_next~mstart_doy+molt_dur,data=molt_dat,family="binomial")
performance::r2(m3)

m_arrive <- lm(arr_doy~br_col+age+sex+bqi_cat+season+mstart_doy+molt_dur,data=molt_dat)
summary(m_arrive)



ggplot(gdr_molt,aes(mstart_doy,molt_dur))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(gdr_molt,aes(sex,age,col=sex))+
  geom_boxplot()
         
summary(m1)
plot(m0)


ggplot(molt_dat,aes(breed_cat,fdive_mean))+
  geom_boxplot()+
  facet_wrap(~br_col+season)

gdr_molt%>%
  filter(breeder_next==1)%>%
  mutate(season=season+1)%>%
ggplot(aes(arrived,fill=br_col))+
  geom_histogram(bins=20)+
  facet_wrap(~season,scales="free_x")+
  ggtitle("arrival date of breeders")
  
fdives_day%>%
ggplot()+
  geom_line(aes(doy,mean,col=factor(season)),lwd=1.2)


molt_dat%>%
  ggplot(aes(season,arr_doy))+
  geom_boxplot()

molt_dat%>%
  mutate(breed_cat = as.numeric(breed_cat))%>%
ggplot(aes(age,breed_cat))+
  geom_point()+
  stat_smooth(method="lm", formula = y~x)
