# estimate variation in travel time (sd)

# library
library(reshape2)
library(dplyr)
library(tidyr)

# load data from results
jsats_dpd <- read.csv("data/JSATS/jsats_dpd_refactor.csv")
length(unique(jsats_dpd$FishID))#785
jsats_dpd$date <- as.Date(jsats_dpd$date_time)
str(jsats_dpd)
length(unique(jsats_dpd$date))

key <- read.csv("data/FishID_key.csv")
jsats_13 <- subset(key, TagType == "JSATS" & Year == 2013)
jsats_14 <- subset(key, TagType == "JSATS" & Year == 2014)
jsats_15 <- subset(key, TagType == "JSATS" & Year == 2015)
jsats_16 <- subset(key, TagType == "JSATS" & Year == 2016)
jsats_17 <- subset(key, TagType == "JSATS" & Year == 2017)

jsats_13_dpd <- merge(jsats_dpd, jsats_13[,c(2,10)], by = "FishID")
length(unique(jsats_13_dpd$FishID))#76
jsats_14_dpd <- merge(jsats_dpd, jsats_14[,c(2,10)], by = "FishID")
length(unique(jsats_14_dpd$FishID))#34
jsats_15_dpd <- merge(jsats_dpd, jsats_15[,c(2,10)], by = "FishID")
length(unique(jsats_15_dpd$FishID))#92
jsats_16_dpd <- merge(jsats_dpd, jsats_16[,c(2,10)], by = "FishID")
length(unique(jsats_16_dpd$FishID))#145 (was expecting 149)
jsats_17_dpd <- merge(jsats_dpd, jsats_17[,c(2,10)], by = "FishID")
length(unique(jsats_17_dpd$FishID))#438 (was expecting 442)

jsats_13_summary <- jsats_13_dpd[,c(1,4,5)] %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

jsats_14_summary <- jsats_14_dpd[,c(1,4,5)] %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

jsats_15_summary <- jsats_15_dpd[,c(1,4,5)] %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

jsats_16_summary <- jsats_16_dpd[,c(1,4,5)] %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

jsats_17_summary <- jsats_17_dpd[,c(1,4,5)] %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

jsats_13_matrix <- dcast(jsats_13_summary, FishID~date, fun = sum, fill = NA_real_)
jsats_14_matrix <- dcast(jsats_14_summary, FishID~date, fun = sum, fill = NA_real_)
jsats_15_matrix <- dcast(jsats_15_summary, FishID~date, fun = sum, fill = NA_real_)
jsats_16_matrix <- dcast(jsats_16_summary, FishID~date, fun = sum, fill = NA_real_)
jsats_17_matrix <- dcast(jsats_17_summary, FishID~date, fun = sum, fill = NA_real_)

#SD
spread_jsats_13 <- transform(jsats_13_matrix, SD=apply(jsats_13_matrix[c(2:ncol(jsats_13_matrix))],1, sd, na.rm = TRUE))

rel <- names(jsats_13_matrix[-1])[max.col(!is.na(jsats_13_matrix[-1]), "first")]
end <- names(jsats_13_matrix[-ncol(jsats_13_matrix)])[max.col(!is.na(jsats_13_matrix[-ncol(jsats_13_matrix)]), "last")]
sd_jsats_13 <- data.frame(spread_jsats_13[,c(1,89)], rel, end)

head(sd_jsats_13)
str(sd_jsats_13)

spread_jsats_14 <- transform(jsats_14_matrix, SD=apply(jsats_14_matrix[c(2:ncol(jsats_14_matrix))],1, sd, na.rm = TRUE))

rel <- names(jsats_14_matrix[-1])[max.col(!is.na(jsats_14_matrix[-1]), "first")]
end <- names(jsats_14_matrix[-ncol(jsats_14_matrix)])[max.col(!is.na(jsats_14_matrix[-ncol(jsats_14_matrix)]), "last")]
sd_jsats_14 <- data.frame(spread_jsats_14[,c(1,42)], rel, end)

head(sd_jsats_14)
str(sd_jsats_14)

spread_jsats_15 <- transform(jsats_15_matrix, SD=apply(jsats_15_matrix[c(2:ncol(jsats_15_matrix))],1, sd, na.rm = TRUE))

rel <- names(jsats_15_matrix[-1])[max.col(!is.na(jsats_15_matrix[-1]), "first")]
end <- names(jsats_15_matrix[-ncol(jsats_15_matrix)])[max.col(!is.na(jsats_15_matrix[-ncol(jsats_15_matrix)]), "last")]
sd_jsats_15 <- data.frame(spread_jsats_15[,c(1,57)], rel, end)

head(sd_jsats_15)
str(sd_jsats_15)

spread_jsats_16 <- transform(jsats_16_matrix, SD=apply(jsats_16_matrix[c(2:ncol(jsats_16_matrix))],1, sd, na.rm = TRUE))

rel <- names(jsats_16_matrix[-1])[max.col(!is.na(jsats_16_matrix[-1]), "first")]
end <- names(jsats_16_matrix[-ncol(jsats_16_matrix)])[max.col(!is.na(jsats_16_matrix[-ncol(jsats_16_matrix)]), "last")]
sd_jsats_16 <- data.frame(spread_jsats_16[,c(1,75)], rel, end)

head(sd_jsats_16)
str(sd_jsats_16)

spread_jsats_17 <- transform(jsats_17_matrix, SD=apply(jsats_17_matrix[c(2:ncol(jsats_17_matrix))],1, sd, na.rm = TRUE))

rel <- names(jsats_17_matrix[-1])[max.col(!is.na(jsats_17_matrix[-1]), "first")]
end <- names(jsats_17_matrix[-ncol(jsats_17_matrix)])[max.col(!is.na(jsats_17_matrix[-ncol(jsats_17_matrix)]), "last")]
sd_jsats_17 <- data.frame(spread_jsats_17[,c(1,140)], rel, end)

head(sd_jsats_17)
str(sd_jsats_17)

write.csv(sd_jsats_13, "results/SD/JSATS_13.csv")
write.csv(sd_jsats_14, "results/SD/JSATS_14.csv")
write.csv(sd_jsats_15, "results/SD/JSATS_15.csv")
write.csv(sd_jsats_16, "results/SD/JSATS_16.csv")
write.csv(sd_jsats_17, "results/SD/JSATS_17.csv")

# still need to check which fish were dropped from 2016 and 2017
ID_16 <- unique(jsats_16$FishID)
ID_17 <- unique(jsats_17$FishID)

final_16 <- unique(sd_jsats_16$FishID)
final_17 <- unique(sd_jsats_17$FishID)

setdiff(ID_16, final_16) #"CFR2016-111" "SB2016-094"  "WR2016-265"  "WR2016-555"
setdiff(ID_17, final_17) #"ARF2017-064" "WR2017-020"  "WR2017-158"  "WR2017-493"

check_jstat <- c(setdiff(ID_16, final_16), setdiff(ID_17, final_17))
key[(key$FishID %in% check_jstat), ]
