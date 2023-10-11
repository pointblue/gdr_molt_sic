# Script to plot N dives per day for every bird during molt period along with the period that was estimated using "molt criteria"
# current criteris is average of fewer than 15 dives per day for 7 straight days


library(tidyverse)
library(data.table)
library(gridExtra)



dive <-
  fread(
    "data/diveStats_ds_all_filt.csv"
  )

# # GDR deploy file
# gdr_depl <-
#   read_csv(
#     "Z:/Informatics/S031/analyses/GDR/data/croz_royds_gdr_depl_all_v2021-08-27.csv"
#   )

molt_tab <-
  read_csv("gdr_molt_dates_v2022-02-10.csv")

bird_ids <- unique(dive$file_id)
plts <- list()

for (i in 1:length(bird_ids)){
  bird_id <- bird_ids[i]
  print(paste(i,bird_id))
  
  dat <- filter(dive,file_id==bird_ids[i])%>%
    group_by(bird_id,season,date)%>%
    summarise(n_dives=n(),.groups="drop")%>%
    mutate(doy = as.numeric(format(date,"%j")))%>%
    filter(doy>28&doy<105)%>%
    left_join(molt_tab, by = c("bird_id","season"))
  
 if(nrow(dat>0)){ 
  p <-ggplot(dat, aes(date, n_dives)) +
    geom_bar(stat = "identity") +
    # xlim(as.Date("2017-01-30"),as.Date("2017-04-30"))+
    ylab("Number of Dives per Day") +
    xlab("Date") + 
    geom_segment(
      aes(
        x = start_molt,
        y = 80,
        xend = end_molt,
        yend = 80
      ),
      col = "orange",
      size = 3
    )+
    ggtitle(bird_ids[i])
  plts[[i]]<-p
  # plot_path <-paste0("Z:/Informatics/S031/analyses/GDR/figures/molt_period_figs/",substr(bird_ids[i],1,17),"_est_molt_period.jpg")
  # jpeg(file=plot_path,units="in",width=7,height=5, res=150)
  # print(p)
  # dev.off()
 }else{
 message(paste("no data for",bird_ids[i]))
 }
}
  

figs<-plts[lapply(plts,length)>0]
marrangeGrob(figs, nrow=3,ncol=3)


# zoom in
ggplot(d_dat, aes(date, n)) +
  geom_bar(stat = "identity", col = col.to[1], fill = col.to[1]) +
  ylab("Number of Foraging Dives per Day") +
  xlab("Date") +
  geom_segment(
    data = d_molt,
    aes(
      x = start_molt,
      y = 80,
      xend = end_molt,
      yend = 80
    ),
    col = col.to[4],
    size = 3
  ) +
  annotate("text",
           label = "Molt period",
           x = as.Date("2017-03-14"),
           y = 100) +
  scale_x_continuous(
    limits = c(as.Date("2017-01-30"), as.Date("2017-04-30")),
    breaks = seq(as.Date("2017-01-30"), as.Date("2017-04-30"), by = 10),
    labels = c(
      "Jan 30",
      "Feb 9",
      "Feb 19",
      "Mar 1",
      "Mar 11",
      "Mar 21",
      "Mar 31",
      "Apr 10",
      "Apr 20",
      "Apr 30"
    )
  ) +
  wsc_theme()
