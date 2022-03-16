# Script to calculate sic summary statistics and make figures of SIC trends in Molt areas
# AS 3/9/2022


# Libraries ####
library(tidyverse)
library(viridis)

# load data ####
sic <- read_csv("Z:/Informatics/S031/analyses/gdr_molt_SIC/data/sic_summary_ssmi_1980-2021.csv")


# Define theme ####

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

col1 <- "#FDE725FF" #yellow
col2 <- "#9856c8" # light purple
col3 <- "#21908CFF" # turquoise



# custom theme
peng_theme <- function() {
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



# plot SIC trends in different regions
# trend in 50% area
p50 <- sic%>%
  pivot_longer(cols=full_hr_molt_sic:west_molt95_sic, names_to="contour",values_to="sic")%>%
  mutate(Contour = factor(contour, labels = c("East 50%","East 95%", "Combined HR", "West 50%", "West 95%")))%>%
  filter(Contour%in%c("East 50%","Combined HR", "West 50%"))%>%
  ggplot(aes(year,sic,col=Contour,fill=Contour, group=Contour))+
  geom_point()+
  geom_line()+
  geom_smooth(se = TRUE, method = lm)+
  scale_color_manual(name="", values=c("East 50%"= col1, "West 50%" = col2, "Combined HR" = col3))+
  scale_fill_manual(name="", values=c("East 50%"= col1, "West 50%" = col2, "Combined HR" = col3))+
  peng_theme()+
  ylab("Sea Ice Concentration (%)")+
  xlab("Year")

print(p50)

ggplot_build(p50)

# trend in 95% area
p95 <- sic%>%
  pivot_longer(cols=full_hr_molt_sic:west_molt95_sic, names_to="contour",values_to="sic")%>%
  mutate(Contour = factor(contour, labels = c("East 50%","East 95%", "Combined HR", "West 50%", "West 95%")))%>%
  filter(Contour%in%c("East 95%","Combined HR", "West 95%"))%>%
  ggplot(aes(year,sic,col=Contour,fill=Contour, group=Contour))+
  geom_point()+
  geom_line()+
  geom_smooth(se = TRUE, method = loess)+
  scale_color_manual(name="", values=c("East 95%"= col1, "West 95%" = col2, "Combined HR" = col3))+
  scale_fill_manual(name="", values=c("East 95%"= col1, "West 95%" = col2, "Combined HR" = col3))+
  peng_theme()+
  ylab("Sea Ice Concentration (%)")+
  xlab("Year")
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(..eq.label.., ..adj.rr.label.., after_stat(p.value.label), sep = "~~~~~")), 
                        parse = TRUE)+
  ylim(0,60)

print(p95)
#-------------------------------------------------------------------------------------#
##Calculate summary statistics for ssmi time series####
#-------------------------------------------------------------------------------------#

mean_full <- mean(sic$full_hr_molt_sic) # 16.27476
mean_east50 <- mean(sic$east_molt50_sic) # 48.77381
mean_west50 <- mean(sic$west_molt50_sic) # 15.80714
mean_east95 <- mean(sic$east_molt95_sic) # 27.80214
mean_west95 <- mean(sic$west_molt95_sic) # 13.38119
print(c(mean_full,mean_east50,mean_west50, mean_east95, mean_west95))

# max sic years
# max year in full hr
filter(sic, full_hr_molt_sic == max(full_hr_molt_sic)) #2003
# max in east50
filter(sic, east_molt50_sic == max(east_molt50_sic)) #2003
# max in west50
filter(sic, west_molt50_sic == max(west_molt50_sic)) #2003


# min sic years
# min year in full hr
filter(sic, full_hr_molt_sic == min(full_hr_molt_sic)) #2017
# min in east50
filter(sic, east_molt50_sic == min(east_molt50_sic)) #2011
# min in west50
filter(sic, west_molt50_sic == min(west_molt50_sic)) #1991


# "average" year
# East
sic %>%
  filter(abs(east_molt50_sic - mean_east50) == min(abs(east_molt50_sic - mean_east50))) #2006
# West
sic %>%
  filter(abs(west_molt50_sic - mean_west50) == min(abs(west_molt50_sic - mean_west50)))  #1995 
# Full
sic %>%
  filter(abs(full_hr_molt_sic - mean_full) == min(abs(full_hr_molt_sic - mean_full)))  #1987


# range in sic
# range year in full hr
range(sic$full_hr_molt_sic) # 4.20 to 30.21
# range in east50
range(sic$east_molt50_sic) # 0.8 to 90.6
# range in west50
range(sic$west_molt50_sic) # 2.26 to 66.11

ggplot(sic,aes(east_molt50_sic,west_molt50_sic,col=year))+
  geom_point()+
  geom_smooth(method="lm")

cor.test(sic$east_molt50_sic,sic$west_molt50_sic)


# model sic trends

# East 50
# check autocorrelation
acf(sic$east_molt50_sic)
m_e50 <- lm(east_molt50_sic~year,data=sic)
summary(m_e50)
acf(resid(m_e50))# no autocorrelation in residuals

# east 95
# check autocorrelation
acf(sic$east_molt95_sic)
m_e95 <- lm(east_molt95_sic~year,data=sic)
summary(m_e95)
acf(resid(m_e95))# no autocorrelation in residuals
plot(m_e95)# residuals and QQ plot look reasonable

# West 50
w_acf <-acf(sic$west_molt50_sic) # possibly autocorrelated with 5 yr lag
m_w50 <- lm(west_molt50_sic~year,data=sic)
summary(m_w50)
acf(resid(m_w50)) # no autocorrelation in residuals

# West 95
acf(sic$west_molt95_sic) # possibly autocorrelated with 5 yr lag
m_w95 <- lm(west_molt95_sic~year,data=sic)
summary(m_w95)
acf(resid(m_w50)) # no autocorrelation in residuals
plot(m_w95) # QQ plot maybe has some problems
