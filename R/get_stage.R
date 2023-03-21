# library
library(dplyr)
library(imputeTS)
library(devtools)

devtools::install_github("ryanpeek/wateRshedTools")
library(wateRshedTools)

# stage at Rio Vista
stage <- get_cdec("RVB", 1, "H", "2007-02-15", "2017-06-18")

head(stage)
str(stage)
stage$date <- as.Date(stage$datetime)

sum(is.na(stage$value))
plot(stage$date, stage$value) # two very large values

stage_qc <- subset(stage, value < 2000)

plot(stage_qc$date, stage_qc$value) # a few outlier looking values in 2009

stage_09 <- subset(stage_qc, date < as.Date("2009-11-01") & date > as.Date("2009-09-01"))
plot(stage_09$date, stage_09$value) # 1.19 looks like the correct break to remove those values

stage_clean <- subset(stage_qc, value > 1.19)

# check sampling interval
stage_freq <- stage_clean %>%
  group_by(date) %>%
  summarize(n()) # between 1 and 24

#only include those with great than 20 measurements in a day (21/24 = 0.875)
exclude_dates <- subset(stage_freq, `n()` < 20) # drops 38 days

stage_cont = stage_clean[!(stage_clean$date %in% exclude_dates$date), ] #removes 509 values

# make daily
cv <- function(x) 100*( sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

stage_daily <- stage_cont[,c(6,8)] %>%
  group_by(date) %>%
  summarise_each(funs(mean = mean(., na.rm = TRUE), max = max(., na.rm = TRUE), min = min(., na.rm = TRUE), sd = sd(., na.rm = TRUE), cv, n = sum(!is.na(.))))

stage_daily <- subset(stage_daily, n > 20) # still had one, must have been NA values and real dates

# check for missing days
continous_dates <- data.frame (x = 1:3777, date = seq(as.Date('2007-02-15'),as.Date('2017-06-18'),by='day'))
stage_daily_cont <- merge(stage_daily, continous_dates, by = "date", all = TRUE)

stage_daily_NA <- stage_daily_cont[is.na(stage_daily_cont$mean),] # only 51 out of 4000 obs.

stage_daily_NA$group <- cumsum(c(1, diff.Date(stage_daily_NA$date)) >= 2)

stage_daily_NA_summary <- stage_daily_NA %>%
  group_by(group) %>%
  summarise(length = length(group)) %>%
  as.data.frame(stage_daily_NA_summary) # seven or less consecutive missing values

# small enough, comfortable imputing missing data
stage_daily_cont$mean <- na_ma(stage_daily_cont$mean, k = 7, weighting = "exponential", maxgap = Inf)

write.csv(stage_daily_cont[,c(1,2)], "data/stage_dat.csv")
