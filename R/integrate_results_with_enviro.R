# make final data
# data
# sd and pdo
model_dat <- read.csv("results/model_dat.csv")

# daily enviro data
stage <- read.csv("data/stage_dat.csv")
temperature <- read.csv("data/temperature_dat.csv")
flow <- read.csv("data/flow_dat.csv")

head(model_dat)
str(model_dat)

# fix dates
model_dat$rel <- as.Date(model_dat$rel)
model_dat$end <- as.Date(model_dat$end)
stage$date <- as.Date(stage$date)
temperature$date <- as.Date(temperature$date)
flow$date <- as.Date(flow$date)

length(unique(model_dat$FishID))

keeper_dat <- data.frame(FishID = NA, date_min = as.Date("1900-01-01"),  date_max = as.Date("1900-01-01"), temp_mean = NA, temp_sd = NA, flow_mean = NA, flow_sd = NA, stage_sd = NA)

for(i in unique(model_dat$FishID)){
  temp_dat <- subset(model_dat, FishID == i)
  temp_df <- subset(temperature, date >= min(temp_dat$rel) & date <= max(temp_dat$end))
  stage_df <- subset(stage, date >= min(temp_dat$rel) & date <= max(temp_dat$end))
  flow_df <- subset(flow, date >= min(temp_dat$rel) & date <= max(temp_dat$end))
  new_dat <- data.frame(FishID = i, date_min = min(temp_dat$rel), date_max = max(temp_dat$end), temp_mean = mean(temp_df$mean), temp_sd = sd(temp_df$mean), flow_mean = mean(flow_df$sac), flow_sd = sd(flow_df$sac), stage_sd = sd(stage_df$mean))
  keeper_dat <- rbind(keeper_dat, new_dat)
}

head(keeper_dat)
keeper_dat <- keeper_dat[-1,]

model_df <- merge(keeper_dat[,-c(2:3)], model_dat, by = "FishID", all = TRUE)
head(model_df)

sapply(X = model_df, FUN = function(x) sum(is.na(x))) #just the 8 JSATS

write.csv(model_df, "results/model_dat.csv",  row.names=FALSE)
