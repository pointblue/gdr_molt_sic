#Resighting Rates and Sea Ice Concentration in the Molt Area
#November 13, 2021
#Grant and Annie

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



# Plot SIC with return rates ----------------------------------------------
setwd("Z:/Informatics/S031/analyses/gdr_molt_SIC")
# read in resight summary if not in environment already
resight_summary_df <- read_csv("data/resight_summary_no_PB.csv")

#now join with the sea ice concentration data for the molt area for the same time period:
#sic_df<-read.csv("Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/molt_area_hr_amsr_sic_summary_2003-2021.csv")
#sic_df<-read.csv("Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/molt_area_hr_sic_ssmi_amsr_1980-2021.csv")
sic_df <- read_csv("data/sic_summary_ssmi_1980-2021.csv")

sic_rs_df <-
  left_join(resight_summary_df, sic_df, by = c("season" = "year"))

sic_rs_df_g2000 <-
  filter(sic_rs_df, season > 2000) %>% #data before 2000 not exactly plentiful; Also note lack of breeding status info before 2003
  mutate(iceberg = ifelse(season %in% c(2001:2005), 1, 0))
#plot results:
#ggplot(sic_rs_df, aes(x=total_hr_molt_sic, y=prop_br_resight)) +
my.formula <- y  ~  x

# Set up plot formatting ###
# Define theme ###
col1 = "#006C84"
col2 = "#92D7C2"
# "#A0DDC4"
#"#8CCAB8"
# "#74BDAF"
# "#B2DBD5"

cols <- c("CROZ" = col1, "ROYD" = col2)

# custom theme
peng_theme <- function() {
  theme_classic() %+replace%
    theme(
      axis.title.y = element_text(
        size = 12,
        margin = margin(r = 15),
        angle = 90
      ),
      axis.title = element_text(size = 14, margin = margin(
        t = 15,
        r = 0,
        b = 0,
        l = 0
      )),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16)
    )
}


# Plot with ggplot
# first remove data from cape bird
sic_rs_df_g2000 %>%
  filter(colony != "BIRD") %>%
  pivot_longer(
    cols = full_hr_molt_sic:west_molt95_sic,
    names_to = "contour",
    values_to = "sic"
  ) %>%
  ggplot(aes(
    x = sic,
    y = prop_resight,
    col = colony,
    fill = colony
  )) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ contour) +
  #scale_y_continuous(breaks=seq(0,60,by=10)) +
  #scale_x_continuous(breaks=seq(2003,2021,by=2)) +
  ylab("BB seen time t resighted in time t+1\n(proportion)") +
  xlab("Sea Ice Concentration in molt areas (%)") +
  peng_theme() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_color_manual(name = "", values = cols) +
  scale_fill_manual(name = "", values = cols) +
  ggpmisc::stat_poly_eq(formula = y  ~  x,
                        aes(
                          label = paste(
                            ..eq.label..,
                            ..adj.rr.label..,
                            after_stat(p.value.label),
                            sep = " ~  ~  ~  ~  ~ "
                          )
                        ),
                        parse = TRUE)



# model RR against SIC in different regions ####

# Croz models
c_dat <- filter(sic_rs_df_g2000, colony == "CROZ")

# linear models for crozier return rate
m_e50_c <- lm(prop_resight ~ east_molt50_sic, data = c_dat)
summary(m_e50_c)

m_e95_c <- lm(prop_resight ~ east_molt95_sic, data = c_dat)
summary(m_e95_c)

m_w50_c <- lm(prop_resight ~ west_molt50_sic, data = c_dat)
summary(m_w50_c)

m_w95_c <- lm(prop_resight ~ west_molt95_sic, data = c_dat)
summary(m_w95_c)

# linear models for crozier return rate with iceberg
m_e50_ci <- 
  lm(prop_resight ~ east_molt50_sic + iceberg, data = c_dat)
summary(m_e50_ci)

m_e95_ci <-
  lm(prop_resight ~ east_molt95_sic + iceberg, data = c_dat)
summary(m_e95_ci)

m_w50_ci <-
  lm(prop_resight ~ west_molt50_sic + iceberg, data = c_dat)
summary(m_w50_ci)

m_w95_ci <-
  lm(prop_resight ~ west_molt95_sic + iceberg, data = c_dat)
summary(m_w95_ci)

# quadratic models
m_e50_c2 <-
  lm(prop_resight  ~  poly(east_molt50_sic, 2), data = c_dat)

m_e95_c2 <-
  lm(prop_resight  ~  poly(east_molt95_sic, 2), data = c_dat)

m_w50_c2 <-
  lm(prop_resight  ~  poly(west_molt50_sic, 2), data = c_dat)

m_w95_c2 <-
  lm(prop_resight  ~  poly(west_molt95_sic, 2), data = c_dat)

m_e50_c2 <-
  lm(prop_resight  ~  poly(east_molt50_sic, 2), data = c_dat)

m_e95_c2 <-
  lm(prop_resight  ~  poly(east_molt95_sic, 2), data = c_dat)

m_w50_c2 <-
  lm(prop_resight  ~  poly(west_molt50_sic, 2), data = c_dat)

m_w95_c2 <-
  lm(prop_resight  ~  poly(west_molt95_sic, 2), data = c_dat)


# #quadratic models with iceberg
# m_e50_c2i <-
#   lm(prop_resight  ~  poly(east_molt50_sic, 2) + iceberg, data = c_dat)
# 
# m_e95_c2i <-
#   lm(prop_resight  ~  poly(east_molt95_sic, 2) + iceberg, data = c_dat)
# 
# m_w50_c2i <-
#   lm(prop_resight  ~  poly(west_molt50_sic, 2) + iceberg, data = c_dat)
# 
# m_w95_c2i <-
#   lm(prop_resight  ~  poly(west_molt95_sic, 2) + iceberg, data = c_dat)
# 
# m_e50_c2i <-
#   lm(prop_resight  ~  poly(east_molt50_sic, 2) + iceberg, data = c_dat)
# 
# m_e95_c2i <-
#   lm(prop_resight  ~  poly(east_molt95_sic, 2) + iceberg, data = c_dat)
# 
# m_w50_c2i <-
#   lm(prop_resight  ~  poly(west_molt50_sic, 2) + iceberg, data = c_dat)
# 
# m_w95_c2i <-
#   lm(prop_resight  ~  poly(west_molt95_sic, 2) + iceberg, data = c_dat)

# null model
m_null_c <- lm(prop_resight ~ 1, data = c_dat)


# data.frame of R2 values
c_r2 <- 
  data.frame(
    model = c("m_e50_c", 
              "m_e50_c2", 
              "m_e95_c",
              "m_e95_c2",
              "m_w50_c",
              "m_w50_c2",
              "m_w95_c",
              "m_w95_c2",
              "m_null_c"),
    adj_r2 = 
      c(summary(m_e50_c)$adj.r.squared,
        summary(m_e50_c2)$adj.r.squared,
        summary(m_e95_c)$adj.r.squared,
        summary(m_e95_c2)$adj.r.squared,
        summary(m_w50_c)$adj.r.squared,
        summary(m_w50_c2)$adj.r.squared,
        summary(m_w95_c)$adj.r.squared,
        summary(m_w95_c2)$adj.r.squared,
        summary(m_null_c)$adj.r.squared
        ))
  #   coef = 
  #     c(summary(m_e50_c)$coefficients,
  #       summary(m_e50_c2)$coefficients,
  #       summary(m_e95_c)$coefficients,
  #       summary(m_e95_c2)$coefficients,
  #       summary(m_w50_c)$coefficients,
  #       summary(m_w50_c2)$coefficients,
  #       summary(m_w95_c)$coefficients,
  #       summary(m_w95_c2)$coefficients),
  #   std_err =
  #     c(summary(m_e50_c)$sigma,
  #       summary(m_e50_c2)$sigma,
  #       summary(m_e95_c)$sigma,
  #       summary(m_e95_c2)$sigma,
  #       summary(m_w50_c)$sigma,
  #       summary(m_w50_c2)$sigma,
  #       summary(m_w95_c)$sigma,
  #       summary(m_w95_c2)$sigma)
  # )

#     
# jtools:: export_summs(m_e50_c,
#                       model.names = "East 50%",
#                       coefs = c("SIC" = "east_molt50_sic")
#                       )


cAIC_tab <-
  bbmle::AICctab(
     m_e50_c,
    m_e50_c2,
    m_e95_c,
    m_e95_c2,
    m_w50_c,
    m_w50_c2,
    m_w95_c,
    m_w95_c2,
    m_null_c,
    nobs = 19,
    weights = TRUE,
    base = TRUE,
    delta = TRUE
  ) %>%
  as.data.frame() %>%
    mutate(model = row.names(.),colony = "C. Crozier") %>%
  left_join(c_r2) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  select(model,
         colony,
         AICc,
         dAICc,
         df,
         weight,
         adj_r2)
         

# write table
# write_csv(cAIC_tab, "results/croz_SIC_rr_noPB_model_tab.csv")


# results from top model
sjPlot::tab_model(m_e95_c)

# broom::tidy(m_e95_c) %>%
#   mutate(across(where(is.numeric), round, 3)) %>% 
#   write_csv( "results/croz_SIC_rr__noPB_top_model_output.csv" )
broom::glance(m_e95_c)



# Royds models ####
r_dat <- filter(sic_rs_df_g2000, colony == "ROYD") %>%
  # filter to first year when at least 10 not breeder/non-breeding birds were seen
  filter(season >2002)

# linear models
m_e50_r <- lm(prop_resight ~ east_molt50_sic, data = r_dat)
summary(m_e50_r)

m_e95_r <- lm(prop_resight ~ east_molt95_sic, data = r_dat)
summary(m_e95_r)

m_w50_r <- lm(prop_resight ~ west_molt50_sic, data = r_dat)
summary(m_w50_r)

m_w95_r <- lm(prop_resight ~ west_molt95_sic, data = r_dat)
summary(m_w95_r)

#quadratic models
m_e50_r2 <- lm(prop_resight ~ poly(east_molt50_sic, 2), data = r_dat)
summary(m_e50_r2)

m_e95_r2 <- lm(prop_resight ~ poly(east_molt95_sic, 2), data = r_dat)
summary(m_e95_r2)

m_w50_r2 <- lm(prop_resight ~ poly(west_molt50_sic, 2), data = r_dat)
summary(m_w50_r2)

m_w95_r2 <- lm(prop_resight ~ poly(west_molt95_sic, 2), data = r_dat)
summary(m_w95_r2)



# 
# # linear models with iceberg
# m_e50_ri <-
#   lm(prop_resight ~ east_molt50_sic + iceberg, data = r_dat)
# 
# m_e95_ri <-
#   lm(prop_resight ~ east_molt95_sic + iceberg, data = r_dat)
# 
# m_w50_ri <-
#   lm(prop_resight ~ west_molt50_sic + iceberg, data = r_dat)
# 
# m_w95_ri <-
#   lm(prop_resight ~ west_molt95_sic + iceberg, data = r_dat)
# 
# 
# #quadratic models with iceberg
# m_e50_r2i <-
#   lm(prop_resight ~ poly(east_molt50_sic, 2) + iceberg, data = r_dat)
# 
# m_e95_r2i <-
#   lm(prop_resight ~ poly(east_molt95_sic, 2) + iceberg, data = r_dat)
# 
# m_w50_r2i <-
#   lm(prop_resight ~ poly(west_molt50_sic, 2) + iceberg, data = r_dat)
# 
# m_w95_r2i <-
#   lm(prop_resight ~ poly(west_molt95_sic, 2) + iceberg, data = r_dat)

# null model
m_null_r <- lm(prop_resight ~ 1, data = r_dat)

# data.frame of R2 values
r_r2 <- 
  data.frame(
    model = c("m_e50_r", 
              "m_e50_r2", 
              "m_e95_r",
              "m_e95_r2",
              "m_w50_r",
              "m_w50_r2",
              "m_w95_r",
              "m_w95_r2",
              "m_null_r"),
    adj_r2 = 
      c(summary(m_e50_r)$adj.r.squared,
        summary(m_e50_r2)$adj.r.squared,
        summary(m_e95_r)$adj.r.squared,
        summary(m_e95_r2)$adj.r.squared,
        summary(m_w50_r)$adj.r.squared,
        summary(m_w50_r2)$adj.r.squared,
        summary(m_w95_r)$adj.r.squared,
        summary(m_w95_r2)$adj.r.squared,
        summary(m_null_r)$adj.r.squared
        ))

rAIC_tab <-
  bbmle::AICctab(
    m_e50_r,
    m_e50_r2,
    m_e95_r,
    m_e95_r2,
    m_w50_r,
    m_w50_r2,
    m_w95_r,
    m_w95_r2,
    m_null_r,
    nobs = 19,
    weights = TRUE,
    base = TRUE,
    delta = TRUE
  ) %>%
  as.data.frame() %>%
  mutate(model = row.names(.),colony = "C. Royds") %>%
  left_join(r_r2) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  select(model,
         colony,
         AICc,
         dAICc,
         df,
         weight,
         adj_r2)

sjPlot::tab_model(m_e95_r)

# # write table
# write_csv(rAIC_tab, "results/royd_SIC_rr_no_PB_model_tab.csv")
# 
# 
# broom::tidy(m_e95_r) %>%
#   mutate(across(where(is.numeric), round, 3)) %>% 
#   write_csv( "results/royd_SIC_rr__noPB_top_model_output.csv" )


# Plot top models ####
p_top_c <-
sic_rs_df_g2000 %>%
  filter(colony == "CROZ")%>%
  pivot_longer(
    cols = full_hr_molt_sic:west_molt95_sic,
    names_to = "contour",
    values_to = "sic"
  ) %>%
  mutate(Contour = factor(
    contour,
    labels = c("East 50%", "East 95%", "Combined HR", "West 50%", "West 95%")
  )) %>%
  filter(Contour %in% c("East 95%")) %>%
  ggplot(aes(
    x = sic,
    y = prop_resight
  )) +
  geom_point(color = col1) +
  geom_smooth(formula = y  ~  x, method = "lm",
              color = col1,
              fill = col1) +
  #scale_y_continuous(breaks=seq(0,60,by=10)) +
  #scale_x_continuous(breaks=seq(2003,2021,by=2)) +
  ylab("Banded Bird Return Rate") +
  xlab("SIC in East 95% Molt Contour (%)") +
  peng_theme() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  scale_color_manual(name = "", values = cols[1]) +
  scale_fill_manual(name = "", values = cols[1]) +
  ylim(0.25, 1) +
  # uncomment if you want r2 and p values to appear on figure
  ggpmisc::stat_poly_eq(
    formula = y  ~  x,
    aes(label = paste(
        ..eq.label..,
      ..adj.rr.label.., after_stat(p.value.label), sep = " ~  ~  ~  ~  ~ "
    )),
    parse = TRUE,
    p.digits = 2,
    rr.digits = 2,
    size = 3,
    # npcx = 0.85,
    # npcy = c(0.12, 0.05, 0.12, 0.05),
    small.p = TRUE
  )

p_top_r <-
  sic_rs_df_g2000 %>%
  filter(colony == "ROYD",season >2002)%>%
  pivot_longer(
    cols = full_hr_molt_sic:west_molt95_sic,
    names_to = "contour",
    values_to = "sic"
  ) %>%
  mutate(Contour = factor(
    contour,
    labels = c("East 50%", "East 95%", "Combined HR", "West 50%", "West 95%")
  )) %>%
  filter(Contour %in% c("West 95%")) %>%
  ggplot(aes(
    x = sic,
    y = prop_resight
  )) +
  geom_point(color = col2) +
  geom_smooth(formula = y  ~  x, method = "lm",
              color = col2,
              fill = col2) +
  #scale_y_continuous(breaks=seq(0,60,by=10)) +
  #scale_x_continuous(breaks=seq(2003,2021,by=2)) +
  ylab("Banded Bird Return Rate") +
  xlab("SIC in West 95% Molt Contour (%)") +
  peng_theme() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  ylim(0.25, 1) +
  ggpmisc::stat_poly_eq(
    formula = y  ~  x,
    aes(label = paste(
      ..eq.label..,
      ..adj.rr.label.., after_stat(p.value.label), sep = " ~  ~  ~  ~  ~ "
    )),
    parse = TRUE,
    p.digits = 2,
    rr.digits = 2,
    size = 3,
    # npcx = 0.85,
    # npcy = c(0.12, 0.05),
    small.p = TRUE
  )
print(p_top_r)

# p_top <- sic_rs_df_g2000 %>%
#   filter(colony != "BIRD") %>%
#   pivot_longer(
#     cols = full_hr_molt_sic:west_molt95_sic,
#     names_to = "contour",
#     values_to = "sic"
#   ) %>%
#   mutate(Contour = factor(
#     contour,
#     labels = c("East 50%", "East 95%", "Combined HR", "West 50%", "West 95%")
#   )) %>%
#   filter(Contour %in% c("East 95%", "West 95%")) %>%
#   ggplot(aes(
#     x = sic,
#     y = prop_resight,
#     col = colony,
#     fill = colony
#   )) +
#   geom_point() +
#   geom_smooth(formula = y  ~  x, method = "lm") +
#   facet_wrap(~  Contour) +
#   #scale_y_continuous(breaks=seq(0,60,by=10)) +
#   #scale_x_continuous(breaks=seq(2003,2021,by=2)) +
#   ylab("Banded Bird Return Rate") +
#   xlab("Sea Ice Concentration in Molt Areas (%)") +
#   peng_theme() +
#   theme(
#     strip.background = element_blank(),
#     strip.text = element_text(size = 14),
#     axis.title.y = element_text(size = 14)
#   ) +
#   scale_color_manual(name = "", values = cols) +
#   scale_fill_manual(name = "", values = cols) +
#   ylim(0, 1) +
#   # uncomment if you want r2 and p values to appear on figure
#   ggpmisc::stat_poly_eq(
#     formula = y  ~  x,
#     aes(label = paste(
#       ..eq.label..,
#       ..adj.rr.label.., after_stat(p.value.label), sep = " ~  ~  ~  ~  ~ "
#     )),
#     parse = TRUE,
#     p.digits = 2,
#     rr.digits = 2,
#     size = 3,
#     npcx = 0.85,
#     npcy = c(0.12, 0.05, 0.12, 0.05),
#     small.p = TRUE
#   )




gridExtra::grid.arrange(p_top_c, p_top_r, nrow = 1)

pdf("figs/cr_rr_m95_regression.pdf",
    width = 7.5,
    height = 4)
print(p_top)
dev.off()


jpeg(
  "figs/cr_rr_m95_regression.jpg",
  width = 7.5,
  height = 4,
  units = "in",
  res = 300
)
print(p_top)
dev.off()



# Proportion of breeders returning
m_e50_c_br <- lm(prop_br_resight ~ east_molt50_sic, data = c_dat)
summary(m_e50_c_br)

m_e95_c_br <- lm(prop_br_resight ~ east_molt95_sic, data = c_dat)
summary(m_e95_c_br)

m_w50_c_br <- lm(prop_br_resight ~ west_molt50_sic, data = c_dat)
summary(m_w50_c_br)

m_w95_c_br <- lm(prop_br_resight ~ west_molt95_sic, data = c_dat)
summary(m_w95_c_br)

#quadratic models
# m_e50_c2 <- lm(prop_resight ~ poly(east_molt50_sic,2),data = c_dat)
# summary(m_e50_c2)
#
# m_e95_c2 <- lm(prop_resight ~ poly(east_molt95_sic,2),data = c_dat)
# summary(m_e95_c2)
#
# m_w50_c2 <- lm(prop_resight ~ poly(west_molt50_sic,2),data = c_dat)
# summary(m_w50_c2)
#
# m_w95_c2 <- lm(prop_resight ~ poly(west_molt95_sic,2),data = c_dat)
# summary(m_w95_c2)


cAIC_tab <-
  bbmle::AICctab(
    m_e50_c,
    m_e50_c2,
    m_e95_c,
    m_e95_c2,
    m_w50_c,
    m_w50_c2,
    m_w95_c,
    m_w95_c2,
    nobs = 21,
    weights = TRUE,
    base = TRUE
  )

# Royds models ####
r_dat <- filter(sic_rs_df_g2000, colony == "ROYD")

# linear models
m_e50_r_br <- lm(prop_br_resight ~ east_molt50_sic, data = r_dat)
summary(m_e50_r_br)

m_e95_r_br <- lm(prop_br_resight ~ east_molt95_sic, data = r_dat)
summary(m_e95_r_br)

m_w50_r_br <- lm(prop_br_resight ~ west_molt50_sic, data = r_dat)
summary(m_w50_r_br)

m_w95_r_br <- lm(prop_br_resight ~ west_molt95_sic, data = r_dat)
summary(m_w95_r_br)

#quadratic models
m_e50_r2 <- lm(prop_resight ~ poly(east_molt50_sic, 2), data = r_dat)
summary(m_e50_r2)

m_e95_r2 <- lm(prop_resight ~ poly(east_molt95_sic, 2), data = r_dat)
summary(m_e95_r2)

m_w50_r2 <- lm(prop_resight ~ poly(west_molt50_sic, 2), data = r_dat)
summary(m_w50_r2)

m_w95_r2 <- lm(prop_resight ~ poly(west_molt95_sic, 2), data = r_dat)
summary(m_w95_r2)

rAIC_tab <-
  bbmle::AICctab(
    m_e50_r,
    m_e50_r2,
    m_e95_r,
    m_e95_r2,
    m_w50_r,
    m_w50_r2,
    m_w95_r,
    m_w95_r2,
    nobs = 21,
    weights = TRUE,
    base = TRUE
  )


# Plot top model ####
sic_rs_df_g2000 %>%
  filter(colony != "BIRD") %>%
  pivot_longer(
    cols = full_hr_molt_sic:west_molt95_sic,
    names_to = "contour",
    values_to = "sic"
  ) %>%
  mutate(Contour = factor(
    contour,
    labels = c("East 50%", "East 95%", "Combined HR", "West 50%", "West 95%")
  )) %>%
  filter(Contour %in% c("East 95%", "West 95%")) %>%
  ggplot(aes(
    x = sic,
    y = prop_br_resight,
    col = colony,
    fill = colony
  )) +
  geom_point() +
  geom_smooth(formula = y  ~  x, method = "lm") +
  facet_wrap(~ Contour) +
  #scale_y_continuous(breaks=seq(0,60,by=10)) +
  #scale_x_continuous(breaks=seq(2003,2021,by=2)) +
  ylab("BB seen time t resighted in time t+1\n(proportion)") +
  xlab("Sea Ice Concentration in molt areas (%)") +
  peng_theme() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_color_manual(name = "", values = cols) +
  scale_fill_manual(name = "", values = cols) +
  ggpmisc::stat_poly_eq(formula = y  ~  x,
                        aes(label = paste(
                          ..adj.rr.label.., after_stat(p.value.label), sep = " ~  ~  ~  ~  ~ "
                        )),
                        parse = TRUE)




# plot time series of Crozier and Royds return rates
sic_rs_df_g2000 %>%
  filter(colony != "BIRD") %>%
  ggplot(aes(
    x = season,
    y = prop_resight,
    col = colony,
    fill = colony
  )) +
  geom_point() +
  geom_smooth(formula = y  ~  x, method = "lm") +
  # facet_wrap( ~ Contour)+
  #scale_y_continuous(breaks=seq(0,60,by=10)) +
  #scale_x_continuous(breaks=seq(2003,2021,by=2)) +
  ylab("BB seen time t resighted in time t+1\n(proportion)") +
  xlab("Year") +
  peng_theme() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_color_manual(name = "", values = cols) +
  scale_fill_manual(name = "", values = cols) +
  ggpmisc::stat_poly_eq(formula = y  ~  x,
                        aes(label = paste(
                          ..adj.rr.label.., after_stat(p.value.label), sep = " ~  ~  ~  ~  ~ "
                        )),
                        parse = TRUE)


# time series of breeder return
sic_rs_df_g2000 %>%
  filter(colony != "BIRD") %>%
  ggplot(aes(
    x = season,
    y = prop_br_resight,
    col = colony,
    fill = colony
  )) +
  geom_point() +
  geom_smooth(formula = y  ~  x, method = "lm") +
  # facet_wrap( ~ Contour)+
  #scale_y_continuous(breaks=seq(0,60,by=10)) +
  #scale_x_continuous(breaks=seq(2003,2021,by=2)) +
  ylab("BB seen time t resighted in time t+1\n(proportion)") +
  xlab("Year") +
  peng_theme() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_color_manual(name = "", values = cols) +
  scale_fill_manual(name = "", values = cols) +
  ggpmisc::stat_poly_eq(formula = y  ~  x,
                        aes(label = paste(
                          ..adj.rr.label.., after_stat(p.value.label), sep = " ~  ~  ~  ~  ~ "
                        )),
                        parse = TRUE)












#
# #model this fit; note options available for SIC data between SSMI and AMSR (the latter not available before 2002 and also not available for 2012)
# #rs_sic_fit<-lm(data=sic_rs_df_g2000,prop_resight ~ ssmi_hr_molt_sic_num) #SSMI
# rs_sic_fit<-lm(data=sic_rs_df_g2000,prop_resight ~ croz_hr_molt_sic) #all SSMI
# summary(rs_sic_fit)
#
#now just breeders
sic_rs_df_g2000 %>%
  filter(colony != "BIRD") %>%
  pivot_longer(
    cols = full_hr_molt_sic:west_molt95_sic,
    names_to = "contour",
    values_to = "sic"
  ) %>%
  mutate(Contour = factor(
    contour,
    labels = c("East 50%", "East 95%", "Combined HR", "West 50%", "West 95%")
  )) %>%
  filter(Contour %in% c("East 95%", "West 95%"),
         season > 1999) %>%
  ggplot(aes(x = sic, y = prop_br_resight, col = contour)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap('colony') +
  #scale_y_continuous(breaks=seq(0,60,by=10)) +
  #scale_x_continuous(breaks=seq(2003,2021,by=2)) +
  ylab("Prop ly breeders seen ty") +
  xlab("Sea Ice Concentration in the molt HR area") +
  ylim(0, 1) +
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  ggpmisc::stat_poly_eq(formula = y  ~  x,
                        aes(label = paste(
                          ..adj.rr.label.., after_stat(p.value.label), sep = " ~  ~  ~  ~  ~ "
                        )),
                        parse = TRUE)
#
# #model the fit for breeders only:
# #rs_sic_fit<-lm(data=sic_rs_df,prop_br_resight ~ total_hr_molt_sic)
# sic_rs_df_g2000<-filter(sic_rs_df,season>=2003) #Note lack of breeding status info before 2003
# rs_sic_fit<-lm(data=sic_rs_df_g2000,prop_br_resight ~ croz_hr_molt_sic+colony)
# summary(rs_sic_fit)
#
#
# #compare with resighting matrix values:
# #parker_rs_table<-read.csv("Y:/S031/analyses/aschmidt/gdr_carry_over_effects_molt_date/data/return_rate.csv")
# rs_matrix_df<-read.csv("C:/gballard/S031/analyses/AdultSurvivorship/ResightAnalysis_2021/export_for_kate_090721.csv")
# filter(rs_matrix_df,substr(rs19,1,2)=="CR", type=="CHIC")%>%
#   count(rs19)


# resight rate time series

resight_summary_df %>%
  filter(season>1999) %>%
  ggplot(aes(season, prop_resight, col = colony)) +
  geom_line() +
  geom_smooth(method = "lm") +
  ggpmisc::stat_poly_eq(formula = y  ~  x,
                        aes(
                          label = paste(
                            ..eq.label..,
                            ..adj.rr.label..,
                            after_stat(p.value.label),
                            sep = " ~  ~  ~  ~  ~ "
                          )
                        ),
                        parse = TRUE)
  
           
