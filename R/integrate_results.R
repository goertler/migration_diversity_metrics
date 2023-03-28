# load data

mj_12_sd <- read.csv("results/SD/YoloAce_12.csv")
mj_13_sd <- read.csv("results/SD/YoloAce_13.csv")
ybus_sd <- read.csv("results/SD/YBUS_16.csv")
jsats_13_sd <- read.csv("results/SD/JSATS_13.csv")
jsats_14_sd <- read.csv("results/SD/JSATS_14.csv")
jsats_15_sd <- read.csv("results/SD/JSATS_15.csv")
jsats_16_sd <- read.csv("results/SD/JSATS_16.csv")
jsats_17_sd <- read.csv("results/SD/JSATS_17.csv")
cm_07_sd <- read.csv("results/SD/CMvemco_07.csv")
cm_08_sd <- read.csv("results/SD/CMvemco_08.csv")
cm_09_sd <- read.csv("results/SD/CMvemco_09.csv")
cm_10_sd <- read.csv("results/SD/CMvemco_10.csv")
cm_11_sd <- read.csv("results/SD/CMvemco_11.csv")

head(mj_12_sd)
head(mj_13_sd)
head(ybus_sd)
head(jsats_13_sd)
head(jsats_14_sd)
head(jsats_15_sd)
head(jsats_16_sd)
head(jsats_17_sd)
head(cm_07_sd)
head(cm_08_sd)
head(cm_09_sd)
head(cm_10_sd)
head(cm_11_sd)

# wasn't consistent with year

sd_all <- rbind(mj_12_sd[,-c(1)], mj_13_sd[,-c(1)], ybus_sd[,-c(1)], jsats_13_sd[,-1], jsats_14_sd[,-1], jsats_15_sd[,-1], jsats_16_sd[,-1], jsats_17_sd[,-1], cm_07_sd[,-1], cm_08_sd[,-1], cm_09_sd[,-1], cm_10_sd[,-1], cm_11_sd[,-1]) # 1806

FishID_key <- read.csv("data/FishID_key.csv")

sd_meta <- merge(FishID_key, sd_all, by = "FishID", all = TRUE)
head(sd_meta) #1814

sd_meta$check_year <- duplicated(sd_meta)
sd_check <- subset(sd_meta, check_year == FALSE)

sum(is.na(sd_meta) == TRUE) # 24
summary(is.na(sd_meta) == TRUE) #8 fish
check <- subset(sd_meta, is.na(SD) == TRUE) # JSAT fish from 2016 & 2017
summary(sd_meta) # looks okay
