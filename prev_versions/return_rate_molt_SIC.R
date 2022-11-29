library(tidyverse)

# resight matrix
rs_mat <- read.csv("Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/export_for_kate_090721.csv")%>%
  select(-rs94,-rs95)%>%
  pivot_longer(cols=starts_with("rs"),names_to="year",values_to="status")%>%
  filter(type=="CHIC",avid==0)%>%
  # separate out breeding status code
  mutate(br_status = ifelse(status==0,0,substring(status,3)),
         colony = case_when(
           substr(status,1,2)=="RO" ~ "ROYD",
           substr(status,1,2)=="BI" ~ "BIRD",
           substr(status,1,2)=="CR" ~ "CROZ",
           substr(status,1,2)=="BE" ~ "BEAU"),
         season_yr = ifelse(as.numeric(substr(year,3,4))>80, 
                         as.numeric(paste0(19,substr(year,3,4))),
                         as.numeric(paste0(20,substr(year,3,4)))))%>%
  group_by(bandnumb)%>%
  fill(colony,.direction = "downup")%>%
  # filter(colony%in%c("CROZ","ROYD"))%>%
  ungroup()%>%
  mutate(fid=row_number())


t <- filter(rs_mat, bandnumb==2929)
t <- filter(rr,bandnumb==48760)
# identify which birds seen at multiple colonies
multi_col <- rs_mat%>%
  mutate(colony=factor(colony))%>%
  group_by(bandnumb,colony)%>%
  tally()%>%
  group_by(bandnumb)%>%
  tally()%>%
  filter(n>1)


# gdr birds
gdr_depl <- read.csv("D:/GDR_data/croz_royds_gdr_depl_all_v2021-08-27.csv")%>%
  filter(bird_id<99999)%>%
  distinct(bird_id)


# Calculate return rate of breeders
filt_rows <- rs_mat %>%
  filter(season_yr%in%c(2016,2017,2018,2019)&bandnumb%in%gdr_depl$bird_id)%>%
  select(fid)

rr <- rs_mat%>%
  filter(!fid%in%filt_rows$fid)%>%
  filter(!bandnumb%in%multi_col$bandnumb)%>%
  group_by(bandnumb) %>%
  arrange(season_yr) %>%
  mutate(
    brr = case_when(
      # br_status == "0" & lag(br_status) %in% c("P","N") ~ "seen_not_returned"))
      # br_status %in% c("P","B","N") & lag(br_status) %in% c("P","N") ~ "seen_returned",
      br_status %in% c("P", "B", "N") &
        lag(br_status) == "B" ~ "br_return",
      br_status == "0" & lag(br_status) == "B" ~ "br_not_return"
    ),
    
    rr = case_when(
      br_status %in% c("P", "B", "N") &
        lag (br_status) %in% c("P", "B", "N") ~ "return",
      br_status == "0" &
        lag (br_status) %in% c("P", "B", "N") ~ "not_return"
    )
  )%>%
  filter(!is.na(rr))


# 
brr_summ <- rr%>%
  group_by(season_yr,colony,brr)%>%
  tally()%>%
  pivot_wider(id_cols = c(season_yr,colony),names_from=c(brr), values_from = n)%>%
  mutate(br_rr = br_return/(br_not_return+br_return))%>%
  rename(season=season_yr)

rr_summ <- rr%>% 
  group_by(season_yr,colony,rr)%>%
  tally()%>%
  pivot_wider(id_cols = c(season_yr,colony),names_from=c(rr), values_from = n)%>%
  mutate(rr = return/(not_return+return))%>%
  rename(season=season_yr)
  


ice <- read.csv("Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/molt_area_hr_sic_summary_all_1980-2021.csv")%>%
  rename(season=year)

dat <- ice%>%
  left_join(brr_summ)%>%
  filter(season<2021,colony=="CROZ")

plot(dat$amsr_hr_molt,dat$br_rr)

cor.test(dat$ssmi_hr_molt,dat$br_rr)
cor.test(dat$amsr_hr_molt,dat$br_rr)


m_croz <- lm(br_rr~ssmi_hr_molt,data=filter(dat,colony=="CROZ"))
summary(m_croz)

m_royds <- lm(br_rr~ssmi_hr_molt,data=filter(dat,colony=="ROYD"))
summary(m_royds)


dat%>%
  filter(colony=="CROZ")%>%
ggplot(aes(ssmi_hr_molt,br_rr))+
  geom_point()+
  geom_smooth(method = lm)+
  facet_wrap(~colony)


dat2 <- ice%>%
  left_join(rr_summ)%>%
  filter(season>1999,season<2020)%>%
  filter(colony=="CROZ")


m2_croz <- lm(rr~ssmi_hr_molt, data=dat2)
summary(m2_croz)
  
cor.test(dat2$rr,dat2$ssmi_hr_molt)
summary(m_croz22)

t <- dat2%>%
  filter(colony=="CROZ")



dat2%>%
  # filter(colony=="CROZ")%>%
  ggplot(aes(ssmi_hr_molt,rr))+
  geom_point()+
  geom_smooth(method = lm)

dat2%>%
  ggplot(aes(season,rr,col=colony))+
  geom_point()+
  xlim(1998,2021)+
  geom_smooth(method="lm")


