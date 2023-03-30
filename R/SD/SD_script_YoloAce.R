# estimate variation in travel time (sd)

# library
library(reshape2)
library(dplyr)
library(tidyr)

# load data from results
yoloace_dpd <- read.csv("data/YoloAce/yoloace_dpd_refactored.csv") # 7/29/22

# make matrix
yoloace_dpd$date <- as.Date(yoloace_dpd$date_time)
str(yoloace_dpd)
length(unique(yoloace_dpd$date))

# bring in (real) FishID (currently is actually tagID)
# it is an issue bc R thinks tagID is an integer
key <- read.csv("data/FishID_key.csv")
key_mj_12 <- subset(key, TagType == "Vemco" & Year == 2012)
key_mj_13 <- subset(key, TagType == "Vemco" & Year == 2013) # also separated years

colnames(yoloace_dpd)[2] <- "TagID"
yoloace_dpd_12 <- merge(yoloace_dpd, key_mj_12[,c(2,10)], by = "TagID")
unique(yoloace_dpd_12[,c(1,6)])

yoloace_dpd_13 <- merge(yoloace_dpd, key_mj_13[,c(2,10)], by = "TagID")
unique(yoloace_dpd_13[,c(1,6)])

yoloace_12_summary <- yoloace_dpd_12 %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

yoloace_13_summary <- yoloace_dpd_13 %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

head(yoloace_12_summary)
head(yoloace_13_summary)

yoloace_dpd_matrix_12 <- dcast(yoloace_12_summary, FishID~date, fun = sum, fill = NA_real_)
head(yoloace_dpd_matrix_12)

yoloace_dpd_matrix_13 <- dcast(yoloace_13_summary, FishID~date, fun = sum, fill = NA_real_)
head(yoloace_dpd_matrix_13)

# make SD for each fish
spread_yoloace_12 <- transform(yoloace_dpd_matrix_12, SD = apply(yoloace_dpd_matrix_12[c(2:ncol(yoloace_dpd_matrix_12))], 1, sd, na.rm = TRUE))

rel <- names(yoloace_dpd_matrix_12[-1])[max.col(!is.na(yoloace_dpd_matrix_12[-1]), "first")]
end <- names(yoloace_dpd_matrix_12[-ncol(yoloace_dpd_matrix_12)])[max.col(!is.na(yoloace_dpd_matrix_12[-ncol(yoloace_dpd_matrix_12)]), "last")]

sd_yoloace_12 <- data.frame(spread_yoloace_12[,c(1,20)], rel, end) # was 19, not sure where the extra day is coming from

head(sd_yoloace_12)
str(sd_yoloace_12)

spread_yoloace_13 <- transform(yoloace_dpd_matrix_13, SD = apply(yoloace_dpd_matrix_13[c(2:ncol(yoloace_dpd_matrix_13))], 1, sd, na.rm = TRUE))

rel <- names(yoloace_dpd_matrix_13[-1])[max.col(!is.na(yoloace_dpd_matrix_13[-1]), "first")]
end <- names(yoloace_dpd_matrix_13[-ncol(yoloace_dpd_matrix_13)])[max.col(!is.na(yoloace_dpd_matrix_13[-ncol(yoloace_dpd_matrix_13)]), "last")]

sd_yoloace_13 <- data.frame(spread_yoloace_13[,c(1,29)], rel, end) # was 28, not sure where the extra day is coming from

head(sd_yoloace_13)
str(sd_yoloace_13)

length(unique(sd_yoloace_12$FishID))#30

length(unique(sd_yoloace_13$FishID))#33

write.csv(sd_yoloace_12, "results/SD/YoloAce_12.csv")
write.csv(sd_yoloace_13, "results/SD/YoloAce_13.csv")


