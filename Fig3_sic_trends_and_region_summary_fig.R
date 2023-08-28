# Script to calculate sic summary statistics and make figures of SIC trends in Molt areas
# AS 3/9/2022


# Libraries ####
library(tidyverse)
library(viridis)
# library(ggstar)

# load data ####
sic <- read_csv("data/sic_summary_ssmi_1980-2021_v2023-02-22.csv")
# sic_1719<-read_csv("Z:/Informatics/S031/analyses/gdr_molt_SIC/data/ew_molt_area_sic_summary.csv")


# Define theme ####

# color palette
col1 <- "#FDE725FF" #yellow
col2 <- "#9856c8" # light purple
col3 <- "#21908CFF" # turquoise
col4 <- "#ffff80" # light yellow
col5 <- "#e6caff" # lavender



# custom theme
peng_theme <- function() {
  theme_classic() %+replace%
    theme(
      axis.title.y = element_text(
        size = 10,
        margin = margin(r = 15),
        angle = 90
      ),
      axis.title.x = element_text(size = 10, margin = margin(
        t = 10,
        r = 0,
        b = 0,
        l = 0
      )),
      axis.text = element_text(size = 7),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7)
    )
}



# plot SIC trends in different regions
# trend in 50% area
p50 <- sic%>%
  pivot_longer(cols=contains("sic"), names_to="contour",values_to="sic")%>%
  mutate(Contour = factor(contour, labels = c("East 50%","East 95%", "Combined\nNonbreeding\nRange", "West 50%", "West 95%")))%>%
  filter(Contour%in%c("East 50%","Combined\nNonbreeding\nRange", "West 50%"))%>%
  ggplot(aes(year,sic,col=Contour,fill=Contour, group=Contour))+
  geom_point()+
  geom_line()+
  geom_smooth(se = TRUE, method = lm)+
  scale_color_manual(name="", values=c("East 50%"= col1, "West 50%" = col2, "Combined\nNonbreeding\nRange" = col3))+
  scale_fill_manual(name="", values=c("East 50%"= col1, "West 50%" = col2, "Combined\nNonbreeding\nRange" = col3))+
  peng_theme()+
  ylab("Sea Ice Concentration (%)")+
  xlab("Year")

print(p50)

ggplot_build(p50)

# trend in 95% area
dat <- sic%>%
  pivot_longer(cols=contains("sic"), names_to="contour",values_to="sic")%>%
  mutate(Contour = factor(contour, labels = c("East 50%","East 95%", "Combined\nNonbreeding\nRange", "West 50%", "West 95%")))%>%
  filter(Contour%in%c("East 95%","Combined\nNonbreeding\nRange", "West 95%"))


p95 <- 
  ggplot(data = dat,aes(year,sic,col=Contour,fill=Contour, group=Contour))+
  geom_point(size = 0.8)+
  geom_line()+
  # ggstar::geom_star(data = filter(dat, year %in% c(2017:2019)),aes(year,sic,col=Contour,fill=Contour, group=Contour),size=2, show.legend = FALSE)+
  # geom_point(data = filter(dat, year %in% c(2017:2019)),aes(year,sic,col=Contour,fill=Contour, group=Contour),size=3, shape = "triangle", show.legend = FALSE)+
  geom_smooth(se = TRUE, method = lm)+
  scale_color_manual(name="", values=c("East 95%"= col1, "West 95%" = col2, "Combined\nNonbreeding\nRange" = col3))+
  scale_fill_manual(name="", values=c("East 95%"= col1, "West 95%" = col2, "Combined\nNonbreeding\nRange" = col3))+
  peng_theme()+
  theme(legend.position = c(0.91,0.91))+
  theme(legend.box.background = element_blank()) +
  ylab("Sea Ice Concentration (%)")+
  xlab("Year")+
  geom_text(aes(label="B",x = 1980, y = 50),color="black",size=6)


print(p95)

# pdf("figs/SIC_molt95_trend_linear_noeq.pdf", width = 7.5,height = 5)
# print(p95)
# dev.off()
# 
# jpeg("figs/SIC_molt95_trend_linear_noeq.jpg", width = 7.5,height = 5, units="in", res=300)
#      print(p95)
#      dev.off()

#-------------------------------------------------------------------------------------#
##Calculate summary statistics for ssmi time series####
#-------------------------------------------------------------------------------------#

mean_full <- mean(sic$full_hr_molt_sic) # 16.27476
mean_east50 <- mean(sic$east_molt50_sic) # 48.77381
mean_west50 <- mean(sic$west_molt50_sic) # 15.80714
mean_east95 <- mean(sic$east_molt95_sic) # 27.80214
mean_west95 <- mean(sic$west_molt95_sic) # 13.38119
print(c(mean_full,mean_east50,mean_west50, mean_east95, mean_west95))


# mean for each year of study


# make data frame so can make bar chart with error bars
summ_df <-sic%>%
  pivot_longer(cols = contains("sic"), names_to = "contour",values_to = "sic")%>%
  mutate(Contour = factor(contour, labels = c("East 50%","East 95%", "Combined\nNonbreeding\nRange", "West 50%", "West 95%")))%>%
  group_by(Contour)%>%
  summarise(mean=mean(sic),se=sd(sic)/sqrt(n())) %>% 
  mutate(symbol = "circle")
  

# summarise data from study
summ_1719 <- sic%>%
  filter(year %in% c(2017:2019))%>%
  # pivot_longer(cols = east_50ma_sic:total_hr_sic,names_to = "contour",values_to = "sic")%>% use this line if using the AMSR data table
  pivot_longer(cols = contains("sic"),names_to = "contour",values_to = "sic")%>%
  mutate(Contour = factor(contour, labels = c("East 50%","East 95%","Combined\nNonbreeding\nRange", "West 50%", "West 95%")))%>%
  group_by(Contour)%>%
  summarise(mean=mean(sic),se=sd(sic)/sqrt(n())) %>% 
  mutate(symbol = "triangle")

p_point <-ggplot()+
  geom_point(data=summ_df,aes(Contour,mean, col = Contour, shape = symbol), size = 2) +
  geom_errorbar(data=summ_df,aes(Contour,mean,ymin=mean-se,ymax=mean+se,col=Contour),width=0.08, size=0.6)+

  geom_errorbar(data=summ_1719,aes(Contour,mean,ymin=mean-se,ymax=mean+se,col = Contour),width=0.08, size=0.6)+
  # ggstar::geom_star(data = summ_1719,aes(Contour, mean, col = Contour),size=4) + 
  geom_point (data=summ_1719,aes(Contour,mean,col=Contour,
                                 shape = symbol), size=2)+
  scale_color_manual(name="Study mean", values=c("East 50%" = "#9d8900", 
                                                 "East 95%"= "gold",
                                                 "Combined\nNonbreeding\nRange" = "#005f59", 
                                                 "West 50%" = "#50005a", 
                                                 "West 95%" = col2), 
                     guide =  "none") +
  scale_fill_manual(name="Long-term mean", values=c("East 50%" = col1, "East 95%"= col4,  "Combined\nNonbreeding\nRange" = col3,"West 50%" = col2, "West 95%" = col5),
                    guide = "none")+
  scale_shape_manual(name = "", values = c("circle", "triangle"), labels = c("Long-term mean\n(1980-2021)", "This study\n(2017-2019)")) +
  peng_theme()+
  theme(legend.position = c(0.8, 0.9))+
  ylab("Sea Ice Concentration (%)")+
  xlab("Area")+
  geom_text(aes(label="A",x = "East 50%", y = 52),color="black",size=6, nudge_x = -0.4)

p_point


# pdf("figs/SIC_mean_x_contour_bar.pdf", width = 7.5,height = 5)
# print(p_bar)
# dev.off()
# 
# jpeg("figs/SIC_mean_x_contour_bar.jpg", width = 7.5,height = 5, units="in", res=600)
# print(p_bar)
# dev.off()
                     
# 
# # plot barplot and trends together
pdf("figs/revision1/Fig3_rev1_v13.pdf", width = 4.5 ,height = 6)
gridExtra::grid.arrange(p_point,p95)
dev.off()

jpeg("figs/Fig3_rev1.jpg", 
     width = 7.5,height = 10, units = "in", res=300)
gridExtra::grid.arrange(p_point,p95)
dev.off()



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
