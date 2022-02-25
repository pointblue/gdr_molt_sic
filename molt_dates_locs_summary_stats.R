# Summary stats for molt dates and locations
# code started 2022-02-25 by AS
library(tidyverse)


molt_locs <-data.table::fread("Z:/Informatics/S031/analyses/gdr_molt_SIC/data/molt_locs500_2022-02-15.csv")

# summarize molt dates of birds that are included in molt locs table
molt_locs%>%
  filter(doy<86)%>%
  group_by(season)%>%
  summarise(min_doy = min(doy), max_doy = max(doy))

molt_locs%>%
  filter(doy<86)%>%
  group_by(season,bird_id)%>%
  summarise(n=n()/500)%>%
  summary()


# Table with molt and arrival dates estimated from GDR data
molt_tab <-
  read_csv("Z:/Informatics/S031/analyses/GDR/data/gdr_molt_arrival_dates_v2022-02-11.csv")%>%
  mutate(mstart_doy = as.numeric(format(start_molt,"%j")), mend_doy = as.numeric(format(end_molt,"%j")))


# individuals with molt dates that don't have locations
molt_locs_ids <-molt_locs%>%
  group_by(season,bird_id)%>%
  tally()%>%
  select(season,bird_id)

missing_locs <- anti_join(molt_tab,molt_locs_ids)

summary(missing_locs)

filter(molt_locs, bird_id==3698)

# confirm birds missing from locs500 table
locs500 <- data.table::fread(
  "Z:/Informatics/S031/analyses/GDR/data/gdr_locs_final_500_all_yr_v2021-06-03.csv")

# summarise date of first locations for each bird
first_loc_dates <- locs500%>%
  filter(inserted==FALSE)%>%
  group_by(season,bird_fn)%>%
  summarise(first_loc=min(time1))%>%
  mutate(first_loc_doy = as.numeric(format(first_loc,"%j")))
summary(first_loc_dates)


missing_locs_date <- missing_locs%>%
  left_join(first_loc_dates)%>%
  mutate(molt_off = first_loc_doy-mend_doy)
