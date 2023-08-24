##### pre-process #####
total <- read.csv("D:/Imperial College_Master/Project/Final Figure/GPP.csv")

library(zoo)
library(dplyr)

total$FPAR[total$FPAR == ""] <- NA
total$FPAR[total$FPAR == 0] <- NA

total$site_gpp[total$site_gpp == ""] <- NA
total$site_gpp[total$site_gpp == 0] <- NA
total$site_gpp[1]=0
total$modis_gpp[total$modis_gpp == 0] <- NA
total$SIF[total$SIF < 0] <- NA

interval_size <- 365

for (i in 1:nrow(total)) {
  if (i %% 365 == 0&& is.na(total$site_gpp[i])) {total$site_gpp[i] <- 0}}                            
for (i in seq(366, nrow(total), by = interval_size)) {    
  if (is.na(total$site_gpp[i])) {total$site_gpp[i] <- 0}} 

for (i in 1:nrow(total)) {
  if (i %% 365 == 0&& is.na(total$modis_gpp[i])) {total$modis_gpp[i] <- 0}}                            
for (i in seq(366, nrow(total), by = interval_size)) {    
  if (is.na(total$modis_gpp[i])) {total$modis_gpp[i] <- 0}} 

for (i in 1:nrow(total)) {
  if (i %% 365 == 0&& is.na(total$FPAR[i])) {total$FPAR[i] <- 0}}                            
for (i in seq(366, nrow(total), by = interval_size)) {    
  if (is.na(total$FPAR[i])) {total$FPAR[i] <- 0}} 

for (i in 1:nrow(total)) {
  if (i %% 365 == 0&& is.na(total$SIF[i])) {total$SIF[i] <- 0}}                            
for (i in seq(366, nrow(total), by = interval_size)) {    
  if (is.na(total$SIF[i])) {total$SIF[i] <- 0}} 

#interplotation
grouped_site <- split(total$site_gpp, rep(1:(nrow(total)/365), each = 365))
in_site <- lapply(grouped_site, na.approx)
site_vector <- unlist(in_site)
grouped_modis <- split(total$modis_gpp, rep(1:(nrow(total)/365), each = 365))
in_modis <- lapply(grouped_modis, na.approx)
modis_vector <- unlist(in_modis)
grouped_FPAR <- split(total$FPAR, rep(1:(nrow(total)/365), each = 365))
in_FPAR <- lapply(grouped_FPAR, na.approx)
FPAR_vector <- unlist(in_FPAR)
grouped_SIF <- split(total$SIF, rep(1:(nrow(total)/365), each = 365))
in_SIF <- lapply(grouped_SIF, na.approx)
SIF_vector <- unlist(in_SIF)

Interpolation <- data.frame(site_gpp = site_vector, modis_gpp = modis_vector, SIF = SIF_vector, FPAR = FPAR_vector)
Interpolation$Time <- total$Time
Interpolation$Site <- total$site
Interpolation$LC <- total$LandCover

write.csv(Interpolation, file = "D:/Imperial College_Master/Project/Final Figure/Interplotion.csv", row.names = FALSE)


# smoothing
Interpolation <- Interpolation %>%
  group_by(Group = rep(1:(n() %/% 365), each = 365, length.out = n())) %>%
  mutate(site = rollmean(site_gpp, k = 60, align = "center", fill = NA),
         modis = rollmean(modis_gpp, k = 60, align = "center", fill = NA),
         FPAR1 = rollmean(FPAR, k = 60, align = "center", fill = NA),
         SIF1 = rollmean(SIF, k = 60, align = "center", fill = NA))
write.csv(Interpolation, file = "D:/Imperial College_Master/Project/Final Figure/SM60.csv", row.names = FALSE)



##### Detection #####
## FD
DE<- read.csv("D:/Imperial College_Master/Project/Final Figure/SM60.csv")

# Derivative
group_size <- 365
total_groups <- length(DE$Time) %/% group_size  
group_indices <- rep(1:total_groups, each = group_size) 

DE$der_site <- ave(DE$site, group_indices, FUN = function(x) c(NA, diff(x)))
DE$der_modis <- ave(DE$modis, group_indices, FUN = function(x) c(NA, diff(x)))
DE$der_FPAR <- ave(DE$FPAR1, group_indices, FUN = function(x) c(NA, diff(x)))
DE$der_SIF <- ave(DE$SIF1, group_indices, FUN = function(x) c(NA, diff(x)))



# SoS & EoS
Modis <- data.frame()
for (i in seq(1, nrow(DE), by = 365)) {
  group <- DE[i:(i + 364), "der_modis"]     
  max_row <- which.max(group) + i - 1 
  min_row <- which.min(group) + i - 1
  max_info <- DE[max_row, ]                
  min_info <- DE[min_row, ]
  Modis <- rbind(Modis, max_info, min_info)}   

Site <- data.frame()
for (i in seq(1, nrow(DE), by = 365)) {
  group <- DE[i:(i + 364), "der_site"]     
  max_row <- which.max(group) + i - 1      
  min_row <- which.min(group) + i - 1
  max_info <- DE[max_row, ]                
  min_info <- DE[min_row, ]
  Site <- rbind(Site, max_info, min_info)}   

FPAR <- data.frame()
for (i in seq(1, nrow(DE), by = 365)) {
  group <- DE[i:(i + 364), "der_FPAR"]     
  max_row <- which.max(group) + i - 1      
  min_row <- which.min(group) + i - 1
  max_info <- DE[max_row, ]                
  min_info <- DE[min_row, ]
  FPAR <- rbind(FPAR, max_info, min_info)}

SIF <- data.frame()
for (i in seq(1, nrow(DE), by = 365)) {
  group <- DE[i:(i + 364), "der_SIF"]     
  max_row <- which.max(group) + i - 1      
  min_row <- which.min(group) + i - 1
  max_info <- DE[max_row, ]                
  min_info <- DE[min_row, ]
  SIF <- rbind(SIF, max_info, min_info)}   

FD_DE <- data.frame(Site$Time, Site$site, Modis$Time, Modis$modis, FPAR$Time, FPAR$FPAR1, SIF$Time, SIF$SIF1, Site$Site)
names(FD_DE)[1] <- "Stime"
names(FD_DE)[3] <- "Mtime"
names(FD_DE)[5] <- "Ftime"
names(FD_DE)[7] <- "Itime"

FD_DE$Stime <- as.Date(as.character(FD_DE$Stime), format = "%Y%m%d")
FD_DE$da_site <- as.integer(format(FD_DE$Stime, "%j"))  
FD_DE$Mtime <- as.Date(as.character(FD_DE$Mtime), format = "%Y%m%d")
FD_DE$da_modis <- as.integer(format(FD_DE$Mtime, "%j"))  
FD_DE$Ftime <- as.Date(as.character(FD_DE$Ftime), format = "%Y%m%d")
FD_DE$da_FPAR <- as.integer(format(FD_DE$Ftime, "%j")) 
FD_DE$Itime <- as.Date(as.character(FD_DE$Itime), format = "%Y%m%d")
FD_DE$da_SIF <- as.integer(format(FD_DE$Itime, "%j"))  

FD_DE$season <- rep(c("SoS", "EoS"))

rows <- which(FD_DE$da_site[-1] == FD_DE$da_site[-nrow(FD_DE)]) + 1
rows_to_delete <- unique(c(rows, rows - 1))
FD_DE <- FD_DE[-rows_to_delete,]

write.csv(FD_DE, file = "D:/Imperial College_Master/Project/Final Figure/first_derivation.csv", row.names = FALSE)



## 动态阈值
DE<- read.csv("D:/Imperial College_Master/Project/Final Figure/SM60.csv")


# DE
DE <- DE %>%
  mutate(group = rep(1:(n() %/% 365), each = 365)) %>%
  group_by(group) %>%
  mutate(diff_site = site - min(site, na.rm = TRUE)) %>%
  mutate(diff_modis = modis - min(modis, na.rm = TRUE)) %>%
  mutate(diff_FPAR = FPAR1 - min(FPAR1, na.rm = TRUE)) %>%
  mutate(diff_SIF = SIF1 - min(SIF1, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-group)
# %
DE <- DE %>%
  mutate(group = rep(1:(n() %/% 365), each =365)) %>%
  group_by(group) %>%
  mutate(per_site = diff_site / max(diff_site, na.rm = TRUE) * 100) %>%
  mutate(per_modis = diff_modis / max(diff_modis, na.rm = TRUE) *100) %>%
  mutate(per_FPAR = diff_FPAR / max(diff_FPAR, na.rm = TRUE) *100) %>%
  mutate(per_SIF = diff_SIF / max(diff_SIF, na.rm = TRUE) *100) %>%
  ungroup() %>%
  select(-group)

#SoS & EoS
DE_new <- DE
DE_new$per_site[is.na(DE_new$per_site) | DE_new$per_site == "" | is.nan(DE_new$per_site)] <- 0
DE_new$per_modis[is.na(DE_new$per_modis) | DE_new$per_modis == "" | is.nan(DE_new$per_modis)] <- 0
DE_new$per_FPAR[is.na(DE_new$per_FPAR) | DE_new$per_FPAR == "" | is.nan(DE_new$per_FPAR)] <- 0
DE_new$per_SIF[is.na(DE_new$per_SIF) | DE_new$per_SIF == "" | is.nan(DE_new$per_SIF)] <- 0

DE_new$group_site <- 1
for (i in 2:nrow(DE_new)) {
  if (DE_new$per_site[i] == 100 | (i-1) %% 365 == 0) {
    DE_new$group_site[i] <- DE_new$group_site[i-1] + 1
  } else {DE_new$group_site[i] <- DE_new$group_site[i-1]}}
sub_site <- split(DE_new, DE_new$group_site)

DE_new$group_modis <- 1
for (i in 2:nrow(DE_new)) {
  if (DE_new$per_modis[i] == 100 | (i-1) %% 365 == 0) {
    DE_new$group_modis[i] <- DE_new$group_modis[i-1] + 1
  } else {DE_new$group_modis[i] <- DE_new$group_modis[i-1]}}
sub_modis <- split(DE_new, DE_new$group_modis)

DE_new$group_FPAR <- 1
for (i in 2:nrow(DE_new)) {
  if (DE_new$per_FPAR[i] == 100 | (i-1) %% 365 == 0) {
    DE_new$group_FPAR[i] <- DE_new$group_FPAR[i-1] + 1
  } else {DE_new$group_FPAR[i] <- DE_new$group_FPAR[i-1]}}
sub_FPAR <- split(DE_new, DE_new$group_FPAR)

DE_new$group_SIF <- 1
for (i in 2:nrow(DE_new)) {
  if (DE_new$per_SIF[i] == 100 | (i-1) %% 365 == 0) {
    DE_new$group_SIF[i] <- DE_new$group_SIF[i-1] + 1
  } else {DE_new$group_SIF[i] <- DE_new$group_SIF[i-1]}}
sub_SIF <- split(DE_new, DE_new$group_SIF)


#a. 30%
P30_site <- data.frame()  
for (i in 1:length(sub_site)) {
  sub_data <- sub_site[[i]]  
  row <- sub_data[which.min(abs(sub_data$per_site - 30)), ]  
  P30_site <- rbind(P30_site, row)}      

P30_modis <- data.frame()  
for (i in 1:length(sub_modis)) {
  sub_data <- sub_modis[[i]]  
  row <- sub_data[which.min(abs(sub_data$per_modis - 30)), ]
  P30_modis <- rbind(P30_modis, row)}

P30_FPAR <- data.frame()  
for (i in 1:length(sub_FPAR)) {
  sub_data <- sub_FPAR[[i]]  
  row <- sub_data[which.min(abs(sub_data$per_FPAR - 30)), ]
  P30_FPAR <- rbind(P30_FPAR, row)}

P30_SIF <- data.frame()  
for (i in 1:length(sub_SIF)) {
  sub_data <- sub_SIF[[i]]  
  row <- sub_data[which.min(abs(sub_data$per_SIF - 30)), ]
  P30_SIF <- rbind(P30_SIF, row)}

P301 <- data.frame(P30_site$Time, P30_site$site, P30_site$Site)
names(P301)[1] <- "Stime"
P301$Stime <- as.Date(as.character(P301$Stime), format = "%Y%m%d")
P301$da_site <- as.integer(format(P301$Stime, "%j"))
write.csv(P301, file = "D:/Imperial College_Master/Project/Final Figure/P301.csv", row.names = FALSE)

P30 <- data.frame(P30_modis$Time, P30_modis$modis, P30_FPAR$Time, P30_FPAR$FPAR1, P30_SIF$Time, P30_SIF$SIF1, P30_modis$Site)
names(P30)[1] <- "Mtime"
names(P30)[3] <- "Ftime"
names(P30)[5] <- "Itime"

P30$Mtime <- as.Date(as.character(P30$Mtime), format = "%Y%m%d")
P30$da_modis <- as.integer(format(P30$Mtime, "%j")) 
P30$Ftime <- as.Date(as.character(P30$Ftime), format = "%Y%m%d")
P30$da_FPAR <- as.integer(format(P30$Ftime, "%j")) 
P30$Itime <- as.Date(as.character(P30$Itime), format = "%Y%m%d")
P30$da_SIF <- as.integer(format(P30$Itime, "%j")) 

P30$season <- rep(c("SoS", "EoS"))
write.csv(P30, file = "D:/Imperial College_Master/Project/Final Figure/P30.csv", row.names = FALSE)


#a. 40%
P40_site <- data.frame() 
for (i in 1:length(sub_site)) {
  sub_data <- sub_site[[i]]  
  row <- sub_data[which.min(abs(sub_data$per_site - 40)), ]  
  P40_site <- rbind(P40_site, row)}      

P40_modis <- data.frame()  
for (i in 1:length(sub_modis)) {
  sub_data <- sub_modis[[i]]  
  row <- sub_data[which.min(abs(sub_data$per_modis - 40)), ]
  P40_modis <- rbind(P40_modis, row)}

P40_FPAR <- data.frame()  
for (i in 1:length(sub_FPAR)) {
  sub_data <- sub_FPAR[[i]]  
  row <- sub_data[which.min(abs(sub_data$per_FPAR - 40)), ]
  P40_FPAR <- rbind(P40_FPAR, row)}

P40_SIF <- data.frame()  
for (i in 1:length(sub_SIF)) {
  sub_data <- sub_SIF[[i]]  
  row <- sub_data[which.min(abs(sub_data$per_SIF - 40)), ]
  P40_SIF <- rbind(P40_SIF, row)}

P401 <- data.frame(P40_site$Time, P40_site$site, P40_site$Site)
names(P401)[1] <- "Stime"
P401$Stime <- as.Date(as.character(P401$Stime), format = "%Y%m%d")
P401$da_site <- as.integer(format(P401$Stime, "%j"))
write.csv(P401, file = "D:/Imperial College_Master/Project/Final Figure/P401.csv", row.names = FALSE)

P40 <- data.frame(P40_modis$Time, P40_modis$modis, P40_FPAR$Time, P40_FPAR$FPAR1, P40_SIF$Time, P40_SIF$SIF1, P40_modis$Site)
names(P40)[1] <- "Mtime"
names(P40)[3] <- "Ftime"
names(P40)[5] <- "Itime"

P40$Mtime <- as.Date(as.character(P40$Mtime), format = "%Y%m%d")
P40$da_modis <- as.integer(format(P40$Mtime, "%j")) 
P40$Ftime <- as.Date(as.character(P40$Ftime), format = "%Y%m%d")
P40$da_FPAR <- as.integer(format(P40$Ftime, "%j")) 
P40$Itime <- as.Date(as.character(P40$Itime), format = "%Y%m%d")
P40$da_SIF <- as.integer(format(P40$Itime, "%j")) 

P40$season <- rep(c("SoS", "EoS"))
write.csv(P40, file = "D:/Imperial College_Master/Project/Final Figure/P40.csv", row.names = FALSE)


#a. 50%
P50_site <- data.frame()  
for (i in 1:length(sub_site)) {
  sub_data <- sub_site[[i]] 
  row <- sub_data[which.min(abs(sub_data$per_site - 50)), ]  
  P50_site <- rbind(P50_site, row)}      

P50_modis <- data.frame()  
for (i in 1:length(sub_modis)) {
  sub_data <- sub_modis[[i]]  
  row <- sub_data[which.min(abs(sub_data$per_modis - 50)), ]
  P50_modis <- rbind(P50_modis, row)}

P50_FPAR <- data.frame()  
for (i in 1:length(sub_FPAR)) {
  sub_data <- sub_FPAR[[i]]  
  row <- sub_data[which.min(abs(sub_data$per_FPAR - 50)), ]
  P50_FPAR <- rbind(P50_FPAR, row)}

P50_SIF <- data.frame()  
for (i in 1:length(sub_SIF)) {
  sub_data <- sub_SIF[[i]]  
  row <- sub_data[which.min(abs(sub_data$per_SIF - 50)), ]
  P50_SIF <- rbind(P50_SIF, row)}

P501 <- data.frame(P50_site$Time, P50_site$site, P50_site$Site)
names(P501)[1] <- "Stime"
P501$Stime <- as.Date(as.character(P501$Stime), format = "%Y%m%d")
P501$da_site <- as.integer(format(P501$Stime, "%j"))
write.csv(P501, file = "D:/Imperial College_Master/Project/Final Figure/P501.csv", row.names = FALSE)

P50 <- data.frame(P50_modis$Time, P50_modis$modis, P50_FPAR$Time, P50_FPAR$FPAR1, P50_SIF$Time, P50_SIF$SIF1, P50_modis$Site)
names(P50)[1] <- "Mtime"
names(P50)[3] <- "Ftime"
names(P50)[5] <- "Itime"

P50$Mtime <- as.Date(as.character(P50$Mtime), format = "%Y%m%d")
P50$da_modis <- as.integer(format(P50$Mtime, "%j")) 
P50$Ftime <- as.Date(as.character(P50$Ftime), format = "%Y%m%d")
P50$da_FPAR <- as.integer(format(P50$Ftime, "%j")) 
P50$Itime <- as.Date(as.character(P50$Itime), format = "%Y%m%d")
P50$da_SIF <- as.integer(format(P50$Itime, "%j")) 

P50$season <- rep(c("SoS", "EoS"))
write.csv(P50, file = "D:/Imperial College_Master/Project/Final Figure/P50.csv", row.names = FALSE)




#####SOS/EOS#####
library(tidyverse)
data <- read_csv("D:/Imperial College_Master/Project/Final/detection/P30.csv")

grouped_data <- split(data, rep(1:(nrow(data) %/% 2), each = 2))

for (i in 1:length(grouped_data)) {
  group <- grouped_data[[i]]
  if (nrow(group) == 2) {  
    if (group$da_site[2] <= group$da_site[1]) {
      temp <- group[1,]
      group[1,] <- group[2,]
      group[2,] <- temp}}
  grouped_data[[i]] <- group}

for (i in 1:length(grouped_data)) {
  group <- grouped_data[[i]]
  if (nrow(group) == 2) {  
    if (group$da_modis[2] <= group$da_modis[1]) {
      temp <- group[1,]
      group[1,] <- group[2,]
      group[2,] <- temp}}
  grouped_data[[i]] <- group}

for (i in 1:length(grouped_data)) {
  group <- grouped_data[[i]]
  if (nrow(group) == 2) { 
    if (group$da_SIF[2] <= group$da_SIF[1]) {
      temp <- group[1,]
      group[1,] <- group[2,]
      group[2,] <- temp}}
  grouped_data[[i]] <- group}

result <- do.call(rbind, grouped_data)
write.csv(result, file = "D:/Imperial College_Master/Project/Final/detection/3.csv", row.names = FALSE)
