# library
library(dplyr)
library(imputeTS)

# stage at Rio Vista
stage <- read.csv("data/enviro_data/RVB_Stage.csv")
stage$Date <- as.Date(stage$Date)
stage <- subset(stage, DATA_FLAG != "N")
head(stage)
str(stage)
sum(is.na(stage$VALUE)) #1.2%
plot(stage$Date, stage$VALUE) # a few outlier looking values in 2009

#plot(stage_check$Date, stage_check$VALUE, data = subset(stage, Date < as.Date("2010-01-01") & Date > as.Date("2008-01-01")))
stage_09 <- subset(stage, Date < as.Date("2009-11-01") & Date > as.Date("2009-09-01"))
plot(stage_09$Date, stage_09$VALUE) # 1.19 looks like the correct break to remove those values

stage_clean <- subset(stage, VALUE > 1.19)

# check sampling interval
stage_freq <- stage_clean %>%
  group_by(Date) %>%
  summarize(n()) # between 1 and 24

#only include those with great than 20 measurements in a day (21/24 = 0.875)
exclude_dates <- subset(stage_freq, `n()` < 20) # drops 46 days

stage_cont = stage_clean[!(stage_clean$Date %in% exclude_dates$Date), ] #removes 624 values

# make daily
cv <- function(x) 100*( sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

stage_daily <- stage_cont[,c(7,10)] %>%
  group_by(Date) %>%
  summarise_each(funs(mean = mean(., na.rm = TRUE), max = max(., na.rm = TRUE), min = min(., na.rm = TRUE), sd = sd(., na.rm = TRUE), cv, n = sum(!is.na(.))))

stage_daily <- subset(stage_daily, n > 20) # still had a few, must have been NA values and real dates

# check for missing days
continous_dates <- data.frame (x = 1:3777, Date = seq(as.Date('2007-02-15'),as.Date('2017-06-18'),by='day'))
stage_daily_cont <- merge(stage_daily, continous_dates, by = "Date", all = TRUE)

stage_daily_NA <- stage_daily_cont[is.na(stage_daily_cont$mean),] # only 51 out of 4000 obs.

stage_daily_NA$group <- cumsum(c(1, diff.Date(stage_daily_NA$Date)) >= 2)

stage_daily_NA_summary <- stage_daily_NA %>%
  group_by(group) %>%
  summarise(length = length(group)) %>%
  as.data.frame(stage_daily_NA_summary) # seven or less consecutive missing values

# small enough, comfortable imputing missing data
stage_daily_cont$mean <- na_ma(stage_daily_cont$mean, k = 7, weighting = "exponential", maxgap = Inf)
colnames(stage_daily_cont)[1] <- "date"
write.csv(stage_daily_cont[,-8], "results/SD/stage_daily_mean.csv")
