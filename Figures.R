##### Method #####
Method<- read.csv("D:/Imperial College_Master/Project/Final Figure/site_info.csv")

library(ggplot2)
library(maps)
library(tidyverse)
library(ggspatial)
library(mapproj)

world <- map_data("world")

ggplot() + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "white", fill = "gray85", size = 0.1)+
  xlim(-10,40) + ylim(35,70)+ theme_bw()+
  annotation_north_arrow(style = north_arrow_fancy_orienteering(text_family = "mono",text_face = "bold"), location = "tl", height = unit(1.1, "cm"),width = unit(0.9, "cm"))+
  geom_point(data = Method, alpha = 0.85, aes(x = Lon, y = Lat, colour = PFT), size = 2)+
  geom_point(data = Method, aes(x = Lon, y = Lat), color = "black", size = 2, shape = 21, fill = NA) +
  scale_color_manual(values = c("red", "yellow", "orange", "#32CD32", "#FFBBFF", "lightblue1", "#1E90FF", "#836FFF", "tan4"))
  
scale_colour_brewer(palette = "Accent")

annotation_scale(width_hint = 0.3, pad_y = unit(0.2, "cm"), pad_x = unit(4.4, "cm"))+
  

##### Re: 3 GPP #####
##-python
##Taylor
total365<- read.csv("D:/Imperial College_Master/Project/Final/total365.csv")
library(plotrix)

total365$LC <- ifelse(total365$LandCover == 0, "Land0",
                   ifelse(total365$LandCover == 1, "Land1",
                          ifelse(total365$LandCover == 3, "Land3",
                                 ifelse(total365$LandCover == 4, "Land4", 
                                        ifelse(total365$LandCover == 6, "Land6",
                                               ifelse(total365$LandCover == 7, "Land7", "Unknown"))))))
taylor_data <- data.frame(Reference = total365$site_gpp, Comparison = cbind(total365$modis_gpp, total365$SIF), Group = total365$LC)
taylor.diagram(taylor_data$Reference, cbind(total365$modis_gpp, total365$SIF), groups = taylor_data$Group,
               sd.arcs = TRUE, ref.sd = TRUE, normalize = TRUE, col = "blue",
               pch = 16, cex = 0.8)

plot(total365$SIF, total365$site_gpp)


## curve
data<- read.csv("D:/Imperial College_Master/Project/Final Figure/SM60.csv")
library(tidyverse)
library(ggplot2)

modis <- split(data$modis_gpp, ceiling(seq_along(data$modis_gpp)/365))
modis <- as.data.frame(modis)
modis$ave <- rowMeans(modis[, 1:ncol(modis)], na.rm = TRUE)  # 从第2列开始到最后一列计算平均值

site <- split(data$site_gpp, ceiling(seq_along(data$site_gpp)/365))
site <- as.data.frame(site)
site$ave <- rowMeans(site[, 1:ncol(site)], na.rm = TRUE)  # 从第2列开始到最后一列计算平均值

SIF <- split(data$SIF, ceiling(seq_along(data$SIF)/365))
SIF <- as.data.frame(SIF)
SIF$ave <- rowMeans(SIF[, 1:ncol(SIF)], na.rm = TRUE)  # 从第2列开始到最后一列计算平均值

Aver <- data.frame(site$ave, modis$ave, SIF$ave)
Aver$Date <- seq(from = 1, length.out = nrow(Aver))

ggplot(Aver, aes(x=Date)) + theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                              legend.box.background = element_rect(color = "black"))+
  geom_line(aes(y=site.ave),color="gray20",size=0.5, linetype = "solid")+
  geom_line(aes(y=modis.ave), color = "#EE6A50", size=0.5, linetype = "solid")+
  geom_line(aes(y=SIF.ave*23),color="#6495ED",size=0.5, linetype = "solid")+ #除以3得到与y1相同的刻度范围
  geom_point(aes(y=site.ave),  color = "gray20", size = 0.5)+
  geom_point(aes(y=modis.ave),  color = "#EE6A50", size = 0.5)+
  geom_point(aes(y=SIF.ave*23),  color = "#6495ED", size = 0.5)+
  scale_y_continuous(name = "GPP",
                     sec.axis = sec_axis( trans=~./24, name="SIF"))+
  xlim(0, 365)



Aver$site <- (Aver$site.ave - min(Aver$site.ave)) / (max(Aver$site.ave) - min(Aver$site.ave))
Aver$modis <- (Aver$modis.ave - min(Aver$modis.ave)) / (max(Aver$modis.ave) - min(Aver$modis.ave))
Aver$SIF <- (Aver$SIF.ave - min(Aver$SIF.ave, na.rm = TRUE)) / (max(Aver$SIF.ave, na.rm = TRUE) - min(Aver$SIF.ave, na.rm = TRUE))

write.csv(Aver, file = "D:/Imperial College_Master/Project/Final/hotspot/Average.csv")
# aver
Aver<- read.csv("D:/Imperial College_Master/Project/Final/hotspot/Average.csv")


ggplot(Aver, aes(x = Date), fill = "white") + theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                legend.position = c(0.85, 0.8),  legend.box.background = element_rect(color = "black"))+
  geom_point(aes(y = site, color = "A"), size = 0.8) +
  geom_line(aes(y = site, color = "A"), linetype = "solid", size = 0.6) +

  geom_point(aes(y = modis, color = "B"), size = 0.8) +
  geom_line(aes(y = modis, color = "B"), linetype = "solid", size = 0.6) +

  geom_point(aes(y = SIF, color = "C"), size = 0.8) +
  geom_line(aes(y = SIF, color = "C"), linetype = "solid", size = 0.6) +
  labs(color = "Legend") +
  ylab("Value") +
  scale_color_manual(values = c("A" = "gray20", "B" = "#EE6A50", "C" = "goldenrod3"),
                     labels = c("Label for B", "Label for C", "Label for D"))


## Land Cover
df_a <- read.csv("D:/Imperial College_Master/Project/Final Figure/site_info.csv")
data1 <- read.csv("D:/Imperial College_Master/Project/Final Figure/GPP.csv")
data1$LandCover <- df_a$PFT[match(data1$site, df_a$ID)]
library(ggplot2)
library(dplyr)
library(ggpmisc)

data <- data1[data1$LandCover == "WET", ]
Co <- lm(data$site_gpp ~ data$modis_gpp)
summary(Co)

ggplot(data1, aes(x = modis_gpp, y = site_gpp, color = LandCover))+
  geom_point(shape = 19, color = "darkgray", size=0.5) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.8),  legend.box.background = element_rect(color = "black")) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Linear Regression of Gpp", x = "modis gpp", y = "site gpp") +
  facet_wrap(~ LandCover, nrow = 3) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 20))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red")


data <- data1[data1$LandCover == "WET", ]
Co <- lm(data$site_gpp ~ data$SIF)
summary(Co)

ggplot(data1, aes(x = SIF, y = site_gpp, color = LandCover))+
  geom_point(shape = 19, color = "darkgray", size=0.5) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = c(0.85, 0.8),  legend.box.background = element_rect(color = "black")) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Linear Regression of Gpp", x = "SIF", y = "site gpp") +
  facet_wrap(~ LandCover, nrow = 3) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 20))



##### Re: 3 gpp-detection#####
##Table
data<- read.csv("D:/Imperial College_Master/Project/Final Figure/detection/first_derivation.csv")
SoS <- data[data$season == "SoS",]
EoS <- data[data$season == "EoS",]


thre1 <- quantile(SoS$da_site, 0.975)
thre2 <- quantile(SoS$da_site, 0.025)
thre3 <- quantile(SoS$da_modis, 0.975)
thre4 <- quantile(SoS$da_modis, 0.025)
thre5 <- quantile(SoS$da_SIF, 0.975)
thre6 <- quantile(SoS$da_SIF, 0.025)
SoS <- SoS[SoS$da_site <= thre1 & SoS$da_site >= thre2, ]
SoS <- SoS[SoS$da_modis <= thre3 & SoS$da_modis >= thre4, ]
SoS <- SoS[SoS$da_SIF <= thre5 & SoS$da_SIF >= thre6, ]

SoS <- SoS[!is.na(SoS$P30_site.site),]
SoS <- SoS[!is.na(SoS$P30_modis.modis),]
SoS <- SoS[!is.na(SoS$P30_SIF.SIF1),]

SoS$days1 <- SoS$da_modis - SoS$da_site
SoS$days2 <- SoS$da_SIF - SoS$da_site

mean_B1_S <- mean(SoS$days1, na.rm = TRUE)
mean_B2_S <- mean(SoS$days2, na.rm = TRUE)

mean_AB1_S <- mean(abs(SoS$days1))
mean_AB2_S <- mean(abs(SoS$days2))


SoS$RMSE1 <- (SoS$da_modis - SoS$da_site)^2
SoS$RMSE2 <- (SoS$da_SIF - SoS$da_site)^2

mean_R1_S <- sqrt(mean(SoS$RMSE1, na.rm = TRUE))
mean_R2_S <- sqrt(mean(SoS$RMSE2, na.rm = TRUE))


#EoS
data<- read.csv("D:/Imperial College_Master/Project/Final Figure/detection/30P.csv")
EoS <- data[data$season == "EoS",]

thre1 <- quantile(EoS$da_site, 0.95)
thre2 <- quantile(EoS$da_site, 0.05)
thre3 <- quantile(EoS$da_modis, 0.95)
thre4 <- quantile(EoS$da_modis, 0.05)
thre5 <- quantile(EoS$da_SIF, 0.975)
thre6 <- quantile(EoS$da_SIF, 0.025)
EoS <- EoS[EoS$da_site <= thre1 & EoS$da_site >= thre2, ]
EoS <- EoS[EoS$da_modis <= thre3 & EoS$da_modis >= thre4, ]
EoS <- EoS[EoS$da_SIF <= thre5 & EoS$da_SIF >= thre6, ]

EoS$days1 <- EoS$da_modis - EoS$da_site
EoS$days2 <- EoS$da_SIF - EoS$da_site

mean_B1_E <- mean(EoS$days1, na.rm = TRUE)
mean_B2_E <- mean(EoS$days2, na.rm = TRUE)

mean_AB1_E <- mean(abs(EoS$days1))
mean_AB2_E <- mean(abs(EoS$days2))

EoS$RMSE1 <- (EoS$da_modis - EoS$da_site)^2
EoS$RMSE2 <- (EoS$da_SIF - EoS$da_site)^2

mean_R1_E <- sqrt(mean(EoS$RMSE1, na.rm = TRUE))
mean_R2_E <- sqrt(mean(EoS$RMSE2, na.rm = TRUE))

SoS$P30_modis.LC <-factor(SoS$P30_modis.LC)
mean_values <- aggregate(days2 ~ P30_modis.LC, data = SoS, FUN = function(x) mean(abs(x)))
ggplot(SoS, aes(x = P30_modis.LC, y = days2, fill = P30_modis.LC)) + geom_boxplot(outlier.shape = NA) + theme_bw() 
  labs(title = "SoS Bias days by Land Cover", x = "Land Cover", y = "days") +
  scale_fill_discrete(name = "Land Cover")+
  coord_cartesian(ylim = c(-50, 50))
  geom_text(data = mean_values, aes(x = LandCover, y = days, label = round(days, 1)), vjust = -1.5)



##### Re: SoS #####
SoS<- read.csv("D:/Imperial College_Master/Project/Final Figure/Reason/SoS.csv")
land <- read.csv("D:/Imperial College_Master/Project/Final Figure/site_info.csv")
SoS$LC <- land$PFT[match(SoS$Site, land$ID)]
  
thre1 <- quantile(SoS$site, 0.975)
thre2 <- quantile(SoS$site, 0.025)
thre3 <- quantile(SoS$modis, 0.975)
thre4 <- quantile(SoS$modis, 0.025)
thre5 <- quantile(SoS$SIF, 0.975)
thre6 <- quantile(SoS$SIF, 0.025)
SoS <- SoS[SoS$site <= thre1 & SoS$site >= thre2, ]
SoS <- SoS[SoS$modis <= thre3 & SoS$modis >= thre4, ]
SoS <- SoS[SoS$SIF <= thre5 & SoS$SIF >= thre6, ]
write.csv(SoS, file = "D:/Imperial College_Master/Project/Final Figure/reason/SoS1.csv", row.names = FALSE)




## boxplot
SoS<- read.csv("D:/Imperial College_Master/Project/Final Figure/Reason/SoS2.csv")
SoS$days <- SoS$value - SoS$site
library(tidyverse)
library(rstatix)
library(ggpubr)

SoS_p <- SoS %>% 
  group_by(LC) %>% 
  wilcox_test(formula = days ~ Group) %>% 
  add_significance(p.col = 'p',cutpoints = c(0,0.001,0.01,0.05,1),symbols = c('***','**','*','ns')) %>% 
  add_xy_position(x='LC')

ggplot() + 
  geom_boxplot(data = SoS, mapping = aes(x = LC, y = days, fill = Group), outlier.shape = NA) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values = c('#EE6A50','#6495ED'))+
  stat_pvalue_manual(SoS_p,label = '{p.signif}',tip.length = 0)+
  labs(x = "Land Cover", y = "SoS days")+
  ylim(-50,100)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red3")



## LC
SoS<- read.csv("D:/Imperial College_Master/Project/Final Figure/Reason/SoS1.csv")
library(dplyr)
library(ggplot2)

site <- aggregate(SoS$site, by = list(site = SoS$LC), FUN = function(x) {
  mean_val <- mean(x)
  se_val <- sd(x) / sqrt(length(x))
  return(c(mean = mean_val, SE = se_val))})
modis <- aggregate(SoS$modis, by = list(modis = SoS$LC), FUN = function(x) {
  mean_val <- mean(x)
  se_val <- sd(x) / sqrt(length(x))
  return(c(mean = mean_val, SE = se_val))})
SIF <- aggregate(SoS$SIF, by = list(SIF = SoS$LC), FUN = function(x) {
  mean_val <- mean(x)
  se_val <- sd(x) / sqrt(length(x))
  return(c(mean = mean_val, SE = se_val))})

mean <- data.frame(site$site, site$x, modis$x, SIF$x)
names(mean)[2] <- "site"
names(mean)[4] <- "modis"
names(mean)[6] <- "SIF"

Co <- lm(mean$SIF ~ mean$site)
summary(Co)
ggplot(mean, aes(x = site, color = site.site)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_point(aes(y = modis), shape = 19, size = 2) +  
  geom_point(aes(y = SIF), shape = 17, size = 2) +  
  labs(x = "site", y = "value") +
  geom_smooth(aes(y = modis), color = "dark Gray", method = "lm", se = FALSE, size = 0.8)+
  geom_smooth(aes(y = SIF), color = "chocolate4", method = "lm", se = FALSE, size = 0.8)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "Red")+
  coord_cartesian(xlim = c(30, 160), ylim = c(30, 160))




## Site
SoS<- read.csv("D:/Imperial College_Master/Project/Final Figure/Reason/SoS1.csv")
library(dplyr)

site <- aggregate(SoS$site, by = list(Site = SoS$Site), FUN = function(x) {
  mean_val <- mean(x)
  se_val <- sd(x) / sqrt(length(x))
  return(c(mean = mean_val, SE = se_val))})
modis <- aggregate(SoS$modis, by = list(modis = SoS$Site), FUN = function(x) {
  mean_val <- mean(x)
  se_val <- sd(x) / sqrt(length(x))
  return(c(mean = mean_val, SE = se_val))})
SIF <- aggregate(SoS$SIF, by = list(SIF = SoS$Site), FUN = function(x) {
  mean_val <- mean(x)
  se_val <- sd(x) / sqrt(length(x))
  return(c(mean = mean_val, SE = se_val))})

mean <- data.frame(site$Site, site$x, modis$x, SIF$x)
names(mean)[2] <- "site"
names(mean)[4] <- "modis"
names(mean)[6] <- "SIF"

Co <- lm(mean$site ~ mean$SIF)
summary(Co)
ggplot(mean, aes(x = SIF, y = site)) +
  geom_point() + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "Gray")+
  labs(x = "SoS of SIF", y = "SoS of site", color = "LandCover") +
  annotate("text", x = 40, y = 150, label = "P<0.001, N=60, slope=0.48", color = "black", size = 3)+
  coord_cartesian(xlim = c(0, 150), ylim = c(0, 150))



##map
SoS$days1 <- SoS$modis - SoS$site
SoS$days2 <- SoS$SIF - SoS$site

SoS_m <- aggregate(SoS$days1, by=list(SoS$Site), FUN=mean)
colnames(SoS_m) <- c("site", "Mean_m")
SoS_s <- aggregate(SoS$days2, by=list(SoS$Site), FUN=mean)
colnames(SoS_s) <- c("site", "Mean_s")
SoS <- data.frame(SoS_m$site, SoS_m$Mean_m, SoS_s$Mean_s)

location <- read.csv("D:/Imperial College_Master/Project/Final Figure/site_info.csv")
SoS$Lat <- location$Lat[match(SoS$SoS_m.site, location$ID)] 
SoS$Lon <- location$Lon[match(SoS$SoS_m.site, location$ID)]
SoS$LC <- location$PFT[match(SoS$SoS_m.site, location$ID)]

write.csv(SoS, file = "D:/Imperial College_Master/Project/Final Figure/Reason/SoS_map.csv", row.names = FALSE)
SoS<- read.csv("D:/Imperial College_Master/Project/Final Figure/Reason/SoS_map.csv")


Co <- lm(SoS$SoS_s.Mean_s ~ SoS$SoS_m.Mean_m)
summary(Co)

library(ggplot2)
library(maps)
library(tidyverse)
library(ggspatial)
library(mapproj)

world <- map_data("world")
SoS$modis <- factor(SoS$modis)
SoS$SIF <- factor(SoS$SIF)

ggplot() + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "white", fill = "gray85", size = 0.1)+
  xlim(-10,40) + ylim(35,70)+ theme_bw()+ theme(panel.background = element_rect(fill = "white"))+
  geom_point(data = SoS, aes(x = Lon, y = Lat), color = "black", size = 2.3, shape = 21, fill = NA) +
  geom_point(data = SoS, alpha = 0.85, aes(x = Lon, y = Lat, colour = SIF), size = 2)+
  labs(color = "SIF")+
  scale_color_manual(values = c("tomato3", "orange", "#FFF68F", "thistle1", "lightblue1", "#1874CD", "dark blue"))+
  annotation_north_arrow(style = north_arrow_fancy_orienteering(text_family = "mono",text_face = "bold"), location = "tl", height = unit(1.1, "cm"),width = unit(0.9, "cm"))
  
## correlation
ggplot(SoS, aes(x = SoS$SoS_m.Mean_m, y = SoS$SoS_s.Mean_s)) +
  geom_point() + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "Gray")+
  labs(x = "modis", y = "SIF", color = "LandCover") +
  annotate("text", x = 40, y = 150, label = "P<0.001, N=60, slope=0.48", color = "black", size = 3)+
  coord_cartesian(xlim = c(-70, 50), ylim = c(-70, 50))



##### Re: three points #####
data<- read.csv("D:/Imperial College_Master/Project/Final Figure/SM60.csv")
onesite <- data[data$Site == "FI-Var", ]
library(tidyverse)
library(ggplot2)

modis <- split(onesite$modis, ceiling(seq_along(onesite$modis)/365))
modis <- as.data.frame(modis)
modis$ave <- rowMeans(modis[, 1:ncol(modis)], na.rm = TRUE)  # 从第2列开始到最后一列计算平均值

site <- split(onesite$site, ceiling(seq_along(onesite$site)/365))
site <- as.data.frame(site)
site$ave <- rowMeans(site[, 1:ncol(site)], na.rm = TRUE)  # 从第2列开始到最后一列计算平均值

SIF <- split(onesite$SIF1, ceiling(seq_along(onesite$SIF1)/365))
SIF <- as.data.frame(SIF)
SIF$ave <- rowMeans(SIF[, 1:ncol(SIF)], na.rm = TRUE)  # 从第2列开始到最后一列计算平均值

Aver <- data.frame(site$ave, modis$ave, SIF$ave)
Aver$Date <- seq(from = 1, length.out = nrow(Aver))

ggplot(Aver, aes(x=Date)) + theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                legend.box.background = element_rect(color = "black"))+
  geom_line(aes(y=site.ave),color="gray20",size=0.71, linetype = "solid")+
  geom_line(aes(y=modis.ave), color = "#EE6A50", size=0.71, linetype = "solid")+
  geom_line(aes(y=SIF.ave*25),color="#6495ED",size=0.71, linetype = "solid")+ #除以3得到与y1相同的刻度范围
  geom_point(x = 84, y = 2.1100813, color = "gray10", size = 1.4) +
  geom_point(x = 120, y = 2.6495365, color = "red3", size = 1.4) +
  geom_point(x = 126, y = 0.08575579*25, color = "#104E8B", size = 1.4) +
  geom_errorbar(data = data.frame(x = c(84, 120, 126), y = c(2.1100813, 2.6495365, 0.08575579 * 25), SE = c(1.8, 3, 3.9)),  # 添加误差线
                aes(x = x, y = y, xmin = x - SE, xmax = x + SE), color = c("gray10", "red3", "#104E8B"), width = 1) +
  scale_y_continuous(name = "GPP(BE-Bra)",
                     sec.axis = sec_axis( trans=~./25, name="SIF"))+
  xlim(80, 130)+
  ylim(1, 4)

xlim(30, 380)


#site
point<- read.csv("D:/Imperial College_Master/Project/Final Figure/Reason/SoS1.csv")
point <- point[point$Site == "CH-Dav", ]
point <- point[, -c(5, 6, 7)]

means <- colMeans(point)
se <- apply(point, 2, function(x) sd(x) / sqrt(length(x)))

data_plot <- data.frame(
  Variable = c("site", "modis", "FPAR", "SIF"),
  Mean = means,
  SE = se)
data_plot <- data_plot[-3, ]

ggplot(data_plot, aes(x = Mean, y = Variable)) + theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.box.background = element_rect(color = "black"))+
  geom_point(size = 1.5, color = c("gray10", "#104E8B", "red3")) +  # 绘制散点
  geom_errorbar(aes(xmin = Mean - SE, xmax = Mean + SE), width = 0.2, color = c("gray10", "#104E8B", "red3")) +  # 绘制误差线
  labs(x = "Variable", y = "Mean") +
  xlim(250, 270)





##### Re: EoS #####
EoS<- read.csv("D:/Imperial College_Master/Project/Final Figure/Reason/EoS.csv")
land <- read.csv("D:/Imperial College_Master/Project/Final Figure/site_info.csv")
EoS$LC <- land$PFT[match(EoS$Site, land$ID)]

thre1 <- quantile(EoS$site, 0.95)
thre2 <- quantile(EoS$site, 0.05)
thre3 <- quantile(EoS$modis, 0.975)
thre4 <- quantile(EoS$modis, 0.025)
thre5 <- quantile(EoS$SIF, 0.975)
thre6 <- quantile(EoS$SIF, 0.025)
EoS <- EoS[EoS$site <= thre1 & EoS$site >= thre2, ]
EoS <- EoS[EoS$modis <= thre3 & EoS$modis >= thre4, ]
EoS <- EoS[EoS$SIF <= thre5 & EoS$SIF >= thre6, ]

write.csv(EoS, file = "D:/Imperial College_Master/Project/Final Figure/reason/EoS1.csv", row.names = FALSE)

EoS<- read.csv("D:/Imperial College_Master/Project/Final Figure/Reason/EoS2.csv")

EoS$days <- EoS$value - EoS$site

##boxplot
EoS_p <- EoS %>% 
  group_by(LC) %>% 
  wilcox_test(formula = days ~ Group) %>% 
  add_significance(p.col = 'p',cutpoints = c(0,0.001,0.01,0.05,1),symbols = c('***','**','*','ns')) %>% 
  add_xy_position(x='LC')

ggplot() + 
  geom_boxplot(data = EoS, mapping = aes(x = LC, y = days, fill = Group), outlier.shape = NA) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values = c('#EE6A50','#6495ED'))+
  stat_pvalue_manual(EoS_p,label = '{p.signif}',tip.length = 0)+
  labs(x = "Land Cover", y = "EoS days")+
  ylim(-50,100)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red3")



## LC
EoS<- read.csv("D:/Imperial College_Master/Project/Final Figure/Reason/EoS1.csv")
library(dplyr)

site <- aggregate(EoS$site, by = list(site = EoS$LC), FUN = function(x) {
  mean_val <- mean(x)
  se_val <- sd(x) / sqrt(length(x))
  return(c(mean = mean_val, SE = se_val))})
modis <- aggregate(EoS$modis, by = list(modis = EoS$LC), FUN = function(x) {
  mean_val <- mean(x)
  se_val <- sd(x) / sqrt(length(x))
  return(c(mean = mean_val, SE = se_val))})
SIF <- aggregate(EoS$SIF, by = list(SIF = EoS$LC), FUN = function(x) {
  mean_val <- mean(x)
  se_val <- sd(x) / sqrt(length(x))
  return(c(mean = mean_val, SE = se_val))})

mean <- data.frame(site$site, site$x, modis$x, SIF$x)
names(mean)[2] <- "site"
names(mean)[4] <- "modis"
names(mean)[6] <- "SIF"

Co <- lm(mean$site ~ mean$SIF)
summary(Co)
ggplot(mean, aes(x = site, color = site.site)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_point(aes(y = modis), shape = 19, size = 2) +  
  geom_point(aes(y = SIF), shape = 17, size = 2) +  
  labs(x = "site", y = "value") +
  geom_smooth(aes(y = modis), color = "dark Gray", method = "lm", se = FALSE, size = 0.8)+
  geom_smooth(aes(y = SIF), color = "chocolate4", method = "lm", se = FALSE, size = 0.8)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "Red")+
  coord_cartesian(xlim = c(150, 300), ylim = c(150, 300))

ggplot(mean, aes(x = modis, y = site, color = site.site)) +
  geom_point() + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "Gray")+
  labs(x = "SoS of SIF", y = "SoS of site", color = "LandCover") +
  annotate("text", x = 200, y = 300, label = "P<0.001, N=60, slope=0.48", color = "black", size = 3)+
  coord_cartesian(xlim = c(150, 300), ylim = c(150, 300))


##map
EoS$days1 <- EoS$modis - EoS$site
EoS$days2 <- EoS$SIF - EoS$site

EoS_m <- aggregate(EoS$days1, by=list(EoS$Site), FUN=mean)
colnames(EoS_m) <- c("site", "Mean_m")
EoS_s <- aggregate(EoS$days2, by=list(EoS$Site), FUN=mean)
colnames(EoS_s) <- c("site", "Mean_s")
EoS <- data.frame(EoS_m$site, EoS_m$Mean_m, EoS_s$Mean_s)

location <- read.csv("D:/Imperial College_Master/Project/Final Figure/site_info.csv")
EoS$Lat <- location$Lat[match(EoS$EoS_m.site, location$ID)] 
EoS$Lon <- location$Lon[match(EoS$EoS_m.site, location$ID)]
EoS$LC <- location$PFT[match(EoS$EoS_m.site, location$ID)]

write.csv(EoS, file = "D:/Imperial College_Master/Project/Final Figure/Reason/EoS_map.csv", row.names = FALSE)

EoS<- read.csv("D:/Imperial College_Master/Project/Final Figure/Reason/EoS_map.csv")
Co <- lm(EoS$EoS_s.Mean_s ~ EoS$EoS_m.Mean_m)
summary(Co)

library(ggplot2)
library(maps)
library(tidyverse)
library(ggspatial)
library(mapproj)

world <- map_data("world")
EoS$modis <- factor(EoS$modis)
EoS$SIF <- factor(EoS$SIF)

ggplot() + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "white", fill = "gray85", size = 0.1)+
  xlim(-10,40) + ylim(35,70)+ theme_bw()+ theme(panel.background = element_rect(fill = "white"))+
  geom_point(data = EoS, aes(x = Lon, y = Lat), color = "black", size = 2.3, shape = 21, fill = NA) +
  geom_point(data = EoS, alpha = 0.85, aes(x = Lon, y = Lat, colour = modis), size = 2)+
  labs(color = "modis")+
  scale_color_manual(values = c("orange", "#FFF68F", "thistle1", "lightblue1", "#1874CD", "dark blue"))+
  annotation_north_arrow(style = north_arrow_fancy_orienteering(text_family = "mono",text_face = "bold"), location = "tl", height = unit(1.1, "cm"),width = unit(0.9, "cm"))

## correlation
ggplot(EoS, aes(x = EoS$EoS_m.Mean_m, y = EoS$EoS_s.Mean_s)) +
  geom_point() + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "Gray")+
  labs(x = "modis", y = "SIF", color = "LandCover") +
  annotate("text", x = 40, y = 150, label = "P<0.001, N=60, slope=0.48", color = "black", size = 3)+
  coord_cartesian(xlim = c(-40, 70), ylim = c(-40, 70))


##### point #####
# FI-Var
ggplot(Aver, aes(x=Date)) + theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                              legend.box.background = element_rect(color = "black"))+
  geom_line(aes(y=site.ave),color="gray20",size=0.65, linetype = "solid")+
  geom_line(aes(y=modis.ave), color = "#EE6A50", size=0.65, linetype = "solid")+
  geom_line(aes(y=SIF.ave*25),color="#6495ED",size=0.65, linetype = "solid")+
  geom_point(x = 126, y = 1.2740499, color = "gray10", size = 2) +
  geom_point(x = 123, y = 1.1068703, color = "red3", size = 2) +
  geom_point(x = 127, y = 0.046965577*25, color = "#104E8B", size = 2) +
  geom_errorbar(data = data.frame(x = c(126, 123, 127), y = c(1.2740499, 1.1068703, 0.046965577 * 25), SE = c(1.154701, 2.403701, 1.666667)),  # 添加误差线
                aes(x = x, y = y, xmin = x - SE, xmax = x + SE), color = c("gray10", "red3", "#104E8B"), width = 0.1, size = 0.8) +
  scale_y_continuous(name = "GPP(FI-Var)",
                     sec.axis = sec_axis( trans=~./25, name="SIF"))+
  xlim(112, 134)+
  ylim(0.9, 1.5)


#CH-Dav
ggplot(Aver, aes(x=Date)) + theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                              legend.box.background = element_rect(color = "black"))+
  geom_line(aes(y=site.ave),color="gray20",size=0.65, linetype = "solid")+
  geom_line(aes(y=modis.ave), color = "#EE6A50", size=0.65, linetype = "solid")+
  geom_line(aes(y=SIF.ave*25),color="#6495ED",size=0.65, linetype = "solid")+ #除以3得到与y1相同的刻度范围
  geom_point(x = 84, y = 2.1100813, color = "gray10", size = 2) +
  geom_point(x = 120, y = 2.6495365, color = "red3", size = 2) +
  geom_point(x = 126, y = 0.08575579*25, color = "#104E8B", size = 2) +
  geom_errorbar(data = data.frame(x = c(84, 120, 126), y = c(2.1100813, 2.6495365, 0.08575579 * 25), SE = c(1.8, 3, 3.9)),  # 添加误差线
                aes(x = x, y = y, xmin = x - SE, xmax = x + SE), color = c("gray10", "red3", "#104E8B"), width = 0.7, size = 0.7) +
  scale_y_continuous(name = "GPP(BE-Bra)",
                     sec.axis = sec_axis( trans=~./25, name="SIF"))+
  xlim(80, 130)+
  ylim(1, 4)


# DE-RuS
ggplot(Aver, aes(x=Date)) + theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                              legend.box.background = element_rect(color = "black"))+
  geom_line(aes(y=site.ave),color="gray20",size=0.65, linetype = "solid")+
  geom_line(aes(y=modis.ave), color = "#EE6A50", size=0.65, linetype = "solid")+
  geom_line(aes(y=SIF.ave*25),color="#6495ED",size=0.65, linetype = "solid")+ #除以3得到与y1相同的刻度范围
  geom_point(x = 110, y = 5.617949, color = "gray10", size = 1.4) +
  geom_point(x = 72, y = 2.3364942, color = "red3", size = 1.4) +
  geom_point(x = 77, y = 0.16059704*25, color = "#104E8B", size = 1.4) +
  geom_errorbar(data = data.frame(x = c(110, 72, 77), y = c(5.617949, 2.3364942, 0.16059704 * 25), SE = c(9.722825, 3.3, 3.7)),  # 添加误差线
                aes(x = x, y = y, xmin = x - SE, xmax = x + SE), color = c("gray10", "red3", "#104E8B"), width = 1.1, size = 0.75) +
  scale_y_continuous(name = "GPP(DE-RuS)",
                     sec.axis = sec_axis( trans=~./25, name="SIF"))+
  xlim(60, 130)+
  ylim(1, 7.5)


#DK-Sor
ggplot(Aver, aes(x=Date)) + theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                              legend.box.background = element_rect(color = "black"))+
  geom_line(aes(y=site.ave),color="gray20",size=0.65, linetype = "solid")+
  geom_line(aes(y=modis.ave), color = "#EE6A50", size=0.65, linetype = "solid")+
  geom_line(aes(y=SIF.ave*25),color="#6495ED",size=0.65, linetype = "solid")+ #除以3得到与y1相同的刻度范围
  geom_point(x = 250, y = 7.232729, color = "gray10", size = 2) +
  geom_point(x = 232, y = 4.467021, color = "red3", size = 2) +
  geom_point(x = 212, y = 0.3087591*25, color = "#104E8B", size = 2) +
  geom_errorbar(data = data.frame(x = c(250, 232, 212), y = c(7.232729, 4.467021, 0.3087591 * 25), SE = c(1.93, 2.8, 1.4)),  # 添加误差线
                aes(x = x, y = y, xmin = x - SE, xmax = x + SE), color = c("gray10", "red3", "#104E8B"), width = 1.4, size=0.7) +
  scale_y_continuous(name = "GPP(DK-Sor)",
                     sec.axis = sec_axis( trans=~./25, name="SIF"))+
  xlim(205, 270)+
  ylim(3, 10)


# CH-Oe2
ggplot(Aver, aes(x=Date)) + theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                              legend.box.background = element_rect(color = "black"))+
  geom_line(aes(y=site.ave),color="gray20",size=0.65, linetype = "solid")+
  geom_line(aes(y=modis.ave), color = "#EE6A50", size=0.65, linetype = "solid")+
  geom_line(aes(y=SIF.ave*25),color="#6495ED",size=0.65, linetype = "solid")+ #除以3得到与y1相同的刻度范围
  geom_point(x = 198, y = 4.127431, color = "gray10", size = 2) +
  geom_point(x = 266, y = 4.788865, color = "red3", size = 2) +
  geom_point(x = 271, y = 0.2324959*25, color = "#104E8B", size = 2) +
  geom_errorbar(data = data.frame(x = c(198, 266, 271), y = c(4.127431, 4.788865, 0.2324959 * 25), SE = c(9.4, 1.3, 1.6)),  # 添加误差线
                aes(x = x, y = y, xmin = x - SE, xmax = x + SE), color = c("gray10", "red3", "#104E8B"), width = 1, size = 0.7) +
  scale_y_continuous(name = "GPP(CH-Oe2)",
                     sec.axis = sec_axis( trans=~./25, name="SIF"))+
  xlim(185, 280)+
  ylim(2.5, 7.5)



#BE-Bra
ggplot(Aver, aes(x=Date)) + theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                              legend.box.background = element_rect(color = "black"))+
  geom_line(aes(y=site.ave),color="gray20",size=0.65, linetype = "solid")+
  geom_line(aes(y=modis.ave), color = "#EE6A50", size=0.65, linetype = "solid")+
  geom_line(aes(y=SIF.ave*25),color="#6495ED",size=0.65, linetype = "solid")+ #除以3得到与y1相同的刻度范围
  geom_point(x = 260, y = 4.311666, color = "gray10", size = 2) +
  geom_point(x = 262, y = 4.530595, color = "red3", size = 2) +
  geom_point(x = 261, y = 0.21144338*25, color = "#104E8B", size = 2) +
  geom_errorbar(data = data.frame(x = c(260, 262, 261), y = c(4.311666, 4.530595, 0.21144338 * 25), SE = c(3, 2.6, 1.8)),  # 添加误差线
                aes(x = x, y = y, xmin = x - SE, xmax = x + SE), color = c("gray10", "red3", "#104E8B"), width = 0.35, size = 0.8) +
  scale_y_continuous(name = "GPP(BE-Bra)",
                     sec.axis = sec_axis( trans=~./25, name="SIF"))+
  xlim(245, 275)+
  ylim(3.5, 6)





##### FPAR-Pmodel #####
## SoS
SoS<- read.csv("D:/Imperial College_Master/Project/Final Figure/Reason/SoS.csv")
land <- read.csv("D:/Imperial College_Master/Project/Final Figure/site_info.csv")
SoS$LC <- land$PFT[match(SoS$Site, land$ID)]

SoS <- SoS[SoS$FPAR != 30, ]

thre1 <- quantile(SoS$FPAR, 0.975)
thre2 <- quantile(SoS$FPAR, 0.025)
thre3 <- quantile(SoS$modis, 0.975)
thre4 <- quantile(SoS$modis, 0.025)

SoS <- SoS[SoS$FPAR <= thre1 & SoS$FPAR >= thre2, ]
SoS <- SoS[SoS$modis <= thre3 & SoS$modis >= thre4, ]

SoS$day1 <- SoS$FPAR - SoS$modis
SoS$Group <- "SOS"
library(tidyverse)
library(rstatix)
library(ggpubr)

Co <- lm(SoS$modis ~ SoS$FPAR)
summary(Co)
ggplot(SoS) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_point(aes(x = FPAR, y = modis), shape = 19, size = 1, color = "dark Gray") +  
  labs(x = "FPAR", y = "modis") +
  geom_smooth(aes(x = FPAR, y = modis), color = "black", method = "lm", se = TRUE, size = 0.8)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "Red")+
  coord_cartesian(xlim = c(30, 160), ylim = c(30, 160))



## EoS
EoS<- read.csv("D:/Imperial College_Master/Project/Final Figure/Reason/EoS.csv")
land <- read.csv("D:/Imperial College_Master/Project/Final Figure/site_info.csv")
EoS$LC <- land$PFT[match(EoS$Site, land$ID)]

thre1 <- quantile(EoS$FPAR, 0.975)
thre2 <- quantile(EoS$FPAR, 0.025)
thre3 <- quantile(EoS$modis, 0.975)
thre4 <- quantile(EoS$modis, 0.025)

EoS <- EoS[EoS$FPAR <= thre1 & EoS$FPAR >= thre2, ]
EoS <- EoS[EoS$modis <= thre3 & EoS$modis >= thre4, ]

EoS$day2 <- EoS$FPAR - EoS$modis
EoS$Group <- "EoS"

days <- c(SoS$day1, EoS$day2)
Group <- c(SoS$Group, EoS$Group)
LC <- c(SoS$LC, EoS$LC)
whole <- data.frame(days, Group, LC)

##boxplot
ggplot() + 
  geom_boxplot(data = whole, mapping = aes(x = LC, y = days, fill = Group), outlier.shape = NA) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values = c('#EE6A50','#6495ED'))+
  labs(x = "Land Cover", y = "days bias")+
  ylim(-50,100)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red3")

EoS <- EoS[EoS$FPAR != 334, ]

Co <- lm(EoS$modis ~ EoS$FPAR)
summary(Co)
ggplot(EoS) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_point(aes(x = FPAR, y = modis), shape = 19, size = 1, color = "dark Gray") +  
  labs(x = "FPAR", y = "modis") +
  geom_smooth(aes(x = FPAR, y = modis), color = "black", method = "lm", se = TRUE, size = 0.8)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "Red")+
  coord_cartesian(xlim = c(200, 340), ylim = c(200, 340))
