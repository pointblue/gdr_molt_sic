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
p <- sic%>%
  pivot_longer(cols=full_hr_molt_sic:west_molt95_sic, names_to="contour",values_to="sic")%>%
  mutate(Contour = factor(contour, labels = c("East 50%","East 95%", "Combined HR", "West 50%", "West 95%")))%>%
  filter(Contour%in%c("East 50%","Combined HR", "West 50%"))%>%
  ggplot(aes(year,sic,col=Contour,fill=Contour, group=Contour))+
  geom_point()+
  geom_line()+
  geom_smooth(se = TRUE, method = lm)+
  # scale_color_discrete(type=col.to,name="")+
  # scale_fill_discrete(type=col.to,name="")+
  scale_color_viridis(name="",discrete = TRUE)+
  scale_fill_viridis(name="",discrete = TRUE)+
  peng_theme()+
  ylab("Sea Ice Concentration (%)")+
  xlab("Year")

ggplot_build(p)

p_mlocs <- p<- ggplot()+
  geom_tile(data=summ_SPixDF,aes(x,y,fill=layer))+
  scale_fill_viridis(legend.title)+
  # 1000m isobath
  geom_path(data=iso1000,aes(x = long, y = lat,group=group,col="1000m isobath"),show.legend = TRUE,size=0.75)+
  # add 50% and 95% contours
  geom_path(data=cont_50_poly_clip,size=1,aes(x=long,y=lat, group=group,col="50%"),show.legend = "line")+
  geom_path(data=cont_95_poly_clip,size=1,aes(x=long,y=lat, group=group,col="95%"), show.legend = "line")+
  # mpa boundary
  geom_polygon(data=mpa_t,aes(x=long,y=lat, group=group,col="RSRMPA",linetype = "RSRMPA"),
               show.legend = "line",fill="grey",alpha=0,size=0.8)+
  
  # antarctica coastline
  geom_polygon(
    data = ant,
    aes(x = long, y = lat, group = group),
    fill = "grey90",
    # alpha = 0.3,
    col = "grey50"
  ) +
  # lat lon grid
  geom_path(data=polar_grid,aes(x = long, y = lat,group=group),col="grey80",lwd=0.05,alpha=0.5)+
  # set coord system and limits
  coord_sf(
    crs = proj_ant,
    xlim = c(-1625000,   2075000),
    ylim = c(825000, 3175000),
  )+
  theme_classic()+
  scale_color_manual("",values=c("50%"="green", "95%"="purple","RSRMPA" = "grey85","1000m isobath" = "grey50"),
                     breaks = c("50%", "95%","RSRMPA","1000m isobath"))+
  # scale_linetype_manual(values =c("50%"=1, "95%"=1,"RSRMPA" = 1,"2000m isobath" = 1,"ACC front" = 3))+
  guides(color = guide_legend(override.aes = list(linetype = c(1,1,1,1))))+
  scale_linetype(guide = FALSE)+
  theme(title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(size = 10))+
  xlab(xlab) +
  ylab(ylab) +
  # scale_fill_manual(lims=c(0.0001,1))+
  scale_x_continuous(breaks = c(110,130,180,-130,-110,-100))+
  ggtitle(title)


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

