# data
# dates
source(integrate_results.R)
# enviro data
stage <- read.csv("data/stage_dat.csv")
temperature <- read.csv("data/temperature_dat.csv")
flow <- read.csv("data/flow_dat.csv")

head(sd_meta)
str(sd_meta)

sd_meta$rel <- as.Date(sd_meta$rel)
sd_meta$end <- as.Date(sd_meta$end)

length(unique(sd_meta$FishID))

keeper_dat <- data.frame(FishID = NA, date_min = as.Date("1900-01-01"),  date_max = as.Date("1900-01-01"), temp_mean = NA, temp_sd = NA, stage_mean = NA, stage_sd = NA)

for(i in unique(sd_meta$FishID)){
  temp_dat <- subset(sd_meta, FishID == i)
  temp_df <- subset(temp_daily, date >= min(temp_dat$rel) & date <= max(temp_dat$end))
  stage_df <- subset(stage_daily_cont, date >= min(temp_dat$rel) & date <= max(temp_dat$end))
  new_dat <- data.frame(FishID = i, date_min = min(temp_dat$rel),  date_max = max(temp_dat$end), temp_mean = mean(temp_df$mean), temp_sd = sd(temp_df$mean), stage_mean = mean(stage_df$mean), stage_sd = sd(stage_df$mean))
  keeper_dat <- rbind(keeper_dat, new_dat)
}

head(keeper_dat)
keeper_dat <- keeper_dat[-1,]

model_df <- merge(keeper_dat[,-c(2:3)], sd_meta, by = "FishID", all = TRUE)
head(model_df)

sapply(X = model_df, FUN = function(x) sum(is.na(x))) #just the 8 JSATS
check_sd <- model_df[is.na(model_df$SD),]

# missing fish size

write.csv(model_df[,-6], "results/SD/model_dat.csv")
