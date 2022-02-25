# Carry over effects and molt timing
# Code started by A. Schmidt Sept 2021
#
#
# Figures
#    summarising molt dates
#     molt duration
#     typical dive pattern before and after molt

# Load Libraries ####-------------------------------------------------------------------------------------------------------------
list.of.packages <-
  c(
    "corrplot",
    "tidyverse",
    "lubridate",
    "data.table",
    "lme4",
    "nlme",
    "sjstats",
    "MuMIn",
    "jtools",
    "performance"
  )
# compare to existing packages
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
# install missing packages
if (length(new.packages) > 0) {
  install.packages(new.packages)
}
# load required packages
lapply(list.of.packages, library, character.only = TRUE)



# Load data and create analysis table####------------------------------------------------------------------------------------------------------------------
# GDR deploy file
gdr_depl <-
  read_csv(
    "Z:/Informatics/S031/analyses/GDR/data/croz_royds_gdr_depl_all_v2021-08-27.csv"
  )

# Table with molt and arrival dates estimated from GDR data
molt_tab <-
  read_csv("Z:/Informatics/S031/analyses/GDR/data/gdr_molt_arrival_dates_v2022-02-11.csv")

# join molt dates to gdr_depl
gdr_molt <- gdr_depl %>%
  left_join(molt_tab) %>%
  filter(!is.na(molt_dur)) %>%
  # filter(sex!="U",study=="KA")%>% # think I don't want to actually filter out the U's because they are mostly from Royds
  # add molt DOY column
  mutate(mstart_doy = as.numeric(format(start_molt, "%j")),
         mend_doy = as.numeric(format(end_molt, "%j")),
         year=season+1)






# Set up color palettes for figures ####-------------------------------------------------------------------------------------------------
# # # Color for Crozier
col1 = "#006C84"
col2 = "#B2DBD5" # arctic
col3 = "#5EA8A7" # lagoon

# color palette
col.p <- c("#006C84", "#5EA8A7", "#B2DBD5", "white")
# turquoise to orange
col.to <- c("#006C84", "#B2DBD5", "#e6ceb5", "#ff8324", "#f73c2f")
# 
# # pb 2 color palette,
# col.pb <- c("#BFD730", "#F7941D", "#666666", "#A7A9AC")
# 
# pb_col1 <- "#005BAA"
# pb_col2 <- "#4495D1"



# custom theme
wsc_theme <- function() {
  theme_classic() %+replace%
    theme(
      axis.title.y = element_text(
        size = 12,
        margin = margin(r = 15),
        angle = 90
      ),
      axis.title.x = element_text(size = 12, margin = margin(
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





# summary plots of molt start date ####
gdr_molt %>%
    year=factor(season+1),colony=factor(ifelse(br_col=="CROZ", "C. Crozier","C.Royds")))%>%
  ggplot(aes(x = colony, y = mstart_doy, fill = year))+
  geom_boxplot(col = "grey50")+
  # seabiRd::scale_fill_seabird(palette="chileanskua",discrete = TRUE, name = "Year")+
  # scale_fill_gradientn(colors=col.p,discrete)+
  scale_fill_manual(values = col.to[c(1, 2, 4)], name = "Year") +
  wsc_theme() +
  xlab("Breeding Colony") +
  ylab("Molt Start Date") +
  scale_y_continuous(
    breaks = seq(30, 80, by = 10),
    labels = c("Jan 30", "Feb 9", "Feb 19", "Mar 1", "Mar 11", "Mar 21")
  )




# summarize numbers
molt_tab%>%
  mutate(mstart_doy=as.numeric(format(start_molt, "%j")))%>%
  summarise(mean_start = mean(mstart_doy), min_start = min(mstart_doy), max_start = max(mstart_doy))

molt_tab%>%
  mutate(mstart_doy=as.numeric(format(start_molt, "%j")))%>%
  summarise(mean_dur = mean(molt_dur), min_dur = min(molt_dur), max_dur = max(molt_dur))






# plot of molt duration ####
molt_dat %>%
  ggplot(aes(x = br_col, y = molt_dur, fill = year)) +
  geom_boxplot(col = "grey50") +
  # seabiRd::scale_fill_seabird(palette="chileanskua",discrete = TRUE, name = "Year")+
  # scale_fill_gradientn(colors=col.p,discrete)+
  scale_fill_manual(values = col.pb[c(1, 2, 4)], name = "Year") +
  wsc_theme() +
  xlab("Breeding Colony") +
  ylab("Molt Duration (days)")
scale_y_continuous(
  breaks = seq(30, 80, by = 10),
  labels = c("Jan 30", "Feb 9", "Feb 19", "Mar 1", "Mar 11", "Mar 21")
)


mean(molt_dat$molt_dur)
range(molt_dat$molt_dur)

mean(molt_dat$mstart_doy)
range(molt_dat$mstart_doy)

# # calculate range of dates that includes ~95% of individuals for each year (I'm not sure why I'm doing this???)
# #
# # Calculate early cutoff for each season
# early_cutoff <- molt_dat %>%
#   group_by(season)%>%
#   summarize(molt_start = quantile(mstart_doy,c(0.025)),quant = c("early_cut"))%>%
#   tidyr::pivot_wider(id_cols = season,names_from = quant, values_from=molt_start)
#
# # join to molt dates table and filter individuals that started molting earlier than the cutoff
# mstart_filt <- molt_dat%>%
#   left_join(early_cutoff)%>%
#   filter(mstart_doy>=early_cut)
#
# # start with all data again to calculate 97.5% cut off for end of molt
# late_cutoff<- molt_dat%>%
#   group_by(season)%>%
#   summarize(molt_end = quantile(mend_doy,c(0.975)),quant = c("late_cut"))%>%
#   tidyr::pivot_wider(id_cols = season,names_from = quant, values_from=molt_end)
#
# mend_filt <- mstart_filt%>%
#   left_join(late_cutoff)%>%
#   left_join(early_cutoff)%>%
#   filter(mstart_doy>=early_cut&mend_doy<=late_cut)%>%
#   group_by(season)%>%
#   summarise(start_molt=min(mstart_doy), end_molt = max(mend_doy))
#
# ggplot(molt_dat,aes(mstart_doy))+
#   geom_histogram()+
#   facet_wrap(~season)
#
# mend_filt%>%
#   group_by(season)%>%
#   tally()
#
# molt_dat%>%
#   group_by(season)%>%
#   tally()
#
# write.csv(cutoff,"molt_dates_by_season.csv")
#
#

# Model molt start date ####

options(na.action = "na.fail")

molt_dat_std <- molt_dat %>%
  mutate_at(vars(age, bqi), list(scale)) %>%
  filter(!is.na(bqi))

#test whether random effect needed
gm_re <-
  lmer(
    mstart_doy ~ br_col + sex + age * year + bqi * year + breed_cat * year +
      breed_cat * bqi
    + (1 | bird_id),
    REML = FALSE,
    data = molt_dat_std
  )
gm <-
  lm(mstart_doy ~ br_col + sex + age * year + bqi * year + breed_cat * year +
       breed_cat * bqi,
     data = molt_dat_std)

anova(gm_re, gm)
# no difference between two models so re is not supported
# would need to include re if individuals not adequately described by other individual covariates
# age, sex, quality, breeding status
summary(gm)
performance(gm)


combinations1 <- dredge(gm,
                        extra = c(
                          "R^2",
                          F = function(x)
                            summary(x)$fstatistic[[1]]
                        ),
                        REML = FALSE)

head(combinations1)
combinations1

top_mtim <- lm(mstart_doy ~ age + breed_cat + year, data = molt_dat)
summary(top_mtim)
sjPlot::tab_model(top_mtim)


top_mtim2 <- lm(mstart_doy ~ age * year + breed_cat, data = molt_dat)
summary(top_mtim2)
sjPlot::tab_model(top_mtim2)
# age*year interaction conf interval includes 0



# Effects plot for top molt date model####
jtools::plot_summs(top_mtim, scale = TRUE)

effect_plot(
  top_mtim,
  pred = age,
  interval = TRUE,
  int.type = "confidence",
  data = molt_dat,
  scale = FALSE,
  xlab = "Molt Start Day"
)
jtools::effect_plot(
  top_mtim,
  pred = year,
  interval = TRUE,
  int.type = "confidence",
  data = molt_dat
)

interactions::interact_plot(
  top_mtim2,
  pred = age,
  modx = year,
  data = molt_dat,
  interval = TRUE,
  int.type = "confidence"
)

plot(top_mtim) # model residuals look good


# plot model effects ggplot####
pm_dat <- as.data.frame(effects::effect(term = "age", mod = top_mtim))
p.age <- ggplot() +
  geom_line(data = pm_dat,
            aes(x = age, y = fit),
            color = col.pb[1],
            size = 1) +
  #5
  geom_ribbon(
    data = pm_dat,
    aes(x = age, ymin = lower, ymax = upper),
    alpha = 0.3,
    fill = col.pb[1]
  ) +
  # geom_rug(data=molt_dat,aes(age,mstart_doy),length = unit(0.005, "npc"))+
  #6
  labs(x = "Age", y = "") +
  wsc_theme() +
  scale_y_continuous(
    lim = c(42, 65),
    breaks = seq(40, 65, by = 5),
    labels = c("Feb 9", "Feb 14", "Feb 19", "Feb 24", "Mar 1", "Mar 6")
  )

plot(p.age)


# Effect of breeding status
pm_dat2 <-
  as.data.frame(effects::effect(term = "breed_cat", mod = top_mtim))
p.breed <- ggplot() +
  geom_point(
    data = pm_dat2,
    aes(x = breed_cat, y = fit),
    color = col.pb[2],
    size = 3
  ) +
  #5
  geom_errorbar(
    data = pm_dat2,
    aes(x = breed_cat, ymin = lower, ymax = upper),
    col = col.pb[2],
    width = 0.1,
    size = 1
  ) +
  # geom_rug(data=molt_dat,aes(age,mstart_doy),length = unit(0.005, "npc"))+
  #6
  labs(x = "Breeding Status", y = "Molt Start Date") +
  wsc_theme() +
  scale_y_continuous(
    lim = c(42, 65),
    breaks = seq(40, 65, by = 5),
    labels = c("Feb 9", "Feb 14", "Feb 19", "Feb 24", "Mar 1", "Mar 6")
  ) +
  scale_x_discrete(labels = c("Non-Breeder", "Failed Breeder" , "Successful Breeder"))

plot(p.breed)


# Effect of year
pm_dat3 <- as.data.frame(effects::effect(term = "year", mod = top_mtim))
#
p.year <- ggplot() +
  geom_point(data = pm_dat3,
             aes(x = year, y = fit),
             color = col.pb[3],
             size = 3) +
  #5
  geom_errorbar(
    data = pm_dat3,
    aes(x = year, ymin = lower, ymax = upper),
    col = col.pb[3],
    width = 0.1,
    size = 1
  ) +
  # geom_rug(data=molt_dat,aes(age,mstart_doy),length = unit(0.005, "npc"))+
  #6
  labs(x = "Year", y = "") +
  wsc_theme() +
  scale_y_continuous(
    lim = c(42, 65),
    breaks = seq(40, 65, by = 5),
    labels = c("Feb 9", "Feb 14", "Feb 19", "Feb 24", "Mar 1", "Mar 6")
  )
# scale_x_discrete(labels=c("Non-Breeder","Failed Breeder" ,"Successful Breeder"))

plot(p.year)

gridExtra::grid.arrange(p.age, p.breed, p.year, ncol = 1, nrow = 3)



# replace year in top model with other potential variables ####----------------------------

# Replace year with mean number of foraging dives in 14 days prior to molt
# calculated per individual then averaged for the year
mtim_fdive14 <-
  lm(mstart_doy ~ age + breed_cat + mfdive_14day, data = molt_dat)
summary(mtim_fdive14)
sjPlot::tab_model(mtim_fdive14)

jtools::plot_summs(mtim_fdive14, scale = TRUE)

effect_plot(
  mtim_fdive14,
  pred = mfdive_14day,
  interval = TRUE,
  int.type = "confidence",
  data = molt_dat,
  scale = FALSE,
  xlab = "Molt Start Day"
)
jtools::effect_plot(
  mtim_fdive14,
  pred = breed_cat,
  interval = TRUE,
  int.type = "confidence",
  data = molt_dat
)
# more foraging in 14 days prior associated with later average molt


# replace with mean number of foraging dive in 7 days prior to molt
mtim_fdive7 <- lm(mstart_doy ~ age + breed_cat + mfdive_7day, data = molt_dat)
summary(mtim_fdive7)
sjPlot::tab_model(mtim_fdive7)

jtools::plot_summs(mtim_fdive7, scale = TRUE)
effect_plot(
  mtim_fdive7,
  pred = mfdive_7day,
  interval = TRUE,
  int.type = "confidence",
  data = molt_dat,
  scale = FALSE,
  xlab = "Molt Start Day"
)
# more foraging in 7 days prior associated with later average molt




# Replace year with mean productivity
mtim_prod <- lm(mstart_doy ~ age + breed_cat + mean_prod, data = molt_dat)
summary(mtim_prod)

jtools::plot_summs(mtim_prod, scale = TRUE)
effect_plot(
  mtim_prod,
  pred = mean_prod,
  interval = TRUE,
  int.type = "confidence",
  data = molt_dat,
  scale = FALSE,
  xlab = "Molt Start Day"
)
# not quite significant, but molt tends to be earlier with higher mean productivity


# replace year with foraging trip duration
mtim_tripdur <- lm(mstart_doy ~ age + breed_cat + td5, data = molt_dat)
summary(mtim_tripdur)

jtools::plot_summs(mtim_tripdur, scale = TRUE)
effect_plot(
  mtim_tripdur,
  pred = td4,
  interval = TRUE,
  int.type = "confidence",
  data = molt_dat,
  scale = FALSE,
  xlab = "Molt Start Day"
)
# longer foraging tip duration in week 4 and week 5 associated with later molt start (td 4 slightly better)


# replace year with foraging dive and foraging trip duration?
mtim_comb <- lm(mstart_doy ~ age + breed_cat + td5+mfdive_14day, data = molt_dat)
summary(mtim_comb)
jtools::plot_summs(mtim_comb, scale = TRUE)
effect_plot(
  mtim_comb,
  pred = td5,
  interval = TRUE,
  int.type = "confidence",
  data = molt_dat,
  scale = FALSE,
  xlab = "Molt Start Day",
  plot.points=TRUE
)

plot(molt_dat$td4,molt_dat$mfdive_14day)

AICc(mtim_comb,top_mtim)


# Effect of timing on breeding next year ####-------------------------------------------------------------------------------------------------------------

# logistic regression with breeder next as response
#

br_1 <-
  glm(breeder_next ~ mstart_doy,
      data = molt_dat,
      family = "binomial")
summary(br_1)

effect_plot(
  br_1,
  pred = mstart_doy,
  interval = TRUE,
  int.type = "confidence",
  data = molt_dat,
  plot.points = TRUE,
  scale = FALSE,
  xlab = "Molt Start Day"
)





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# relationship between molt start and duration ####
# r = -0.17 p = 0.03
cor.test(molt_dat$mstart_doy, molt_dat$molt_dur)
mean(molt_dat$molt_dur)
sd(molt_dat$molt_dur)
range(molt_dat$molt_dur)
hist(molt_dat$molt_dur)

mdur_gm <- glmer(
  molt_dur ~ mstart_doy + br_col + age + bqi + breed_cat + year
  + (1 | bird_id),
  family = "poisson",
  data = molt_dat_std
)
summary(mdur_gm)
mdur <- glm(
  molt_dur ~ mstart_doy + br_col + age + sex + bqi + breed_cat + year,
  data = molt_dat_std,
  family = "poisson"
)
summary(mdur)

anova(mdur_gm, mdur)

comb_2 <- dredge(mdur,
                 extra = c(
                   "R^2",
                   F = function(x)
                     summary(x)$fstatistic[[1]]
                 ),
                 REML = FALSE)

head(comb_2)
comb_2

mdur_top <-
  lmer(molt_dur ~ mstart_doy + year + (1 |
                                         bird_id), data = molt_dat_std)
summary(top)

jtools::plot_summs(mdur_top, scale = TRUE)


mdur <- lm(molt_dur ~ mstart_doy + age + breed_cat + year, data = molt_dat)
summary(mdur)

ggplot(molt_dat, aes(mstart_doy, molt_dur)) +
  geom_point(aes(mstart_doy, molt_dur)) +
  geom_smooth(method = "lm")

# birds that start to molt later tend to molt for shorter duration

# test for correlation between molt start date and arrival date
cor.test(molt_dat$mstart_doy, molt_dat$arr_doy) # r = -0.02 p = 0.76
cor.test(molt_dat$mend_doy, molt_dat$arr_doy) # r = -0.02 p = 0.79
# no correlation between molt start or end dates and arrival date the following year

# relationships between molt timing and subsequent breeding status?
cor.test(molt_dat$mstart_doy, molt_dat$breeder_next)


ggplot(gdr_molt, aes(mstart_doy, molt_dur)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"))

ggplot(gdr_molt, aes(sex, age, col = sex)) +
  geom_boxplot()


gdr_molt %>%
  filter(breeder_next == 1) %>%
  mutate(season = season + 1) %>%
  ggplot(aes(arrived, fill = br_col)) +
  geom_histogram(bins = 20) +
  facet_wrap( ~ season, scales = "free_x") +
  ggtitle("arrival date of breeders")



molt_dat %>%
  mutate(breed_cat = breed_cat) %>%
  ggplot(aes(breed_cat, age, fill = breed_cat)) +
  geom_boxplot() +
  facet_wrap( ~ season)
