# get pdo data, but estimated monthly, so needs to be processed differently than other environmental data

# data
# add PDO
pdo <- read.csv("data/PDO.csv") # data from https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat

# need dates
source("R/integrate_results.R")
head(sd_meta)
str(sd_meta)
sd_meta <- na.omit(sd_meta)
sd_meta$rel <- as.Date(sd_meta$rel)
sd_meta$end <- as.Date(sd_meta$end)

month_dat <- sd_meta[,c(1,6,13,14)] %>%
  mutate(month_end = lubridate::month(end),
         year_end  = lubridate::year(end),
         month_rel = lubridate::month(rel),
         year_rel  = lubridate::year(rel))

min(month_dat$month_rel, na.rm = TRUE)
max(month_dat$month_end, na.rm = TRUE)

month_dat$combo <- paste(month_dat$month_rel, "_", month_dat$year_rel, "-", month_dat$month_end, "_", month_dat$year_end)

length(unique(month_dat$combo)) #52

# one month
combo_1 <- c("2 _ 2016 - 2 _ 2016","3 _ 2016 - 3 _ 2016", "3 _ 2013 - 3 _ 2013", "5 _ 2016 - 5 _ 2016", "5 _ 2017 - 5 _ 2017", "4 _ 2013 - 4 _ 2013", "4 _ 2017 - 4 _ 2017", "4 _ 2016 - 4 _ 2016",   "4 _ 2015 - 4 _ 2015", "2 _ 2007 - 2 _ 2007", "12 _ 2007 - 12 _ 2007", "1 _ 2008 - 1 _ 2008","12 _ 2008 - 12 _ 2008", "1 _ 2009 - 1 _ 2009", "12 _ 2009 - 12 _ 2009", "1 _ 2010 - 1 _ 2010", "12 _ 2010 - 12 _ 2010", "1 _ 2011 - 1 _ 2011", "6 _ 2017 - 6 _ 2017", "5 _ 2013 - 5 _ 2013",  "2 _ 2014 - 2 _ 2014", "2 _ 2015 - 2 _ 2015", "2 _ 2017 - 2 _ 2017" ) #23

combo_1_dat = month_dat[(month_dat$combo %in% combo_1), ] #1128 (of 1814)
combo_1_dat <- merge(combo_1_dat, pdo, by = "combo")

# two months
combo_2 <- c("3 _ 2012 - 4 _ 2012",
             "2 _ 2016 - 3 _ 2016",
             "3 _ 2016 - 4 _ 2016",
             "5 _ 2017 - 6 _ 2017",
             "4 _ 2013 - 5 _ 2013",
             "4 _ 2017 - 5 _ 2017",
             "1 _ 2007 - 2 _ 2007",
             "12 _ 2007 - 1 _ 2008",
             "1 _ 2008 - 2 _ 2008",
             "12 _ 2008 - 1 _ 2009",
             "1 _ 2009 - 2 _ 2009",
             "12 _ 2009 - 1 _ 2010",
             "1 _ 2010 - 2 _ 2010",
             "12 _ 2010 - 1 _ 2011",
             "1 _ 2011 - 2 _ 2011",
             "4 _ 2016 - 5 _ 2016",
             "2 _ 2013 - 3 _ 2013",
             "2 _ 2014 - 3 _ 2014",
             "2 _ 2015 - 3 _ 2015",
             "2 _ 2017 - 3 _ 2017")

combo_2_dat = month_dat[(month_dat$combo %in% combo_2), ] #630

combo_2_dat$label <- paste(combo_2_dat$month_rel, "_", combo_2_dat$year_rel)
combo_2_dat <- merge(combo_2_dat, pdo[,1:2], by = "label")
colnames(combo_2_dat)[11] <- "rel_pdo"
combo_2_dat <- combo_2_dat[,-1]

combo_2_dat$label <- paste(combo_2_dat$month_end, "_", combo_2_dat$year_end)
combo_2_dat <- merge(combo_2_dat, pdo[,1:2], by = "label")
colnames(combo_2_dat)[12] <- "end_pdo"

combo_2_dat$PDO <- ((combo_2_dat$rel_pdo+combo_2_dat$end_pdo)/2)

# more (should be 56 remaining)
combo_3 <- c("12 _ 2007 - 2 _ 2008",
             "1 _ 2008 - 3 _ 2008",
             "12 _ 2008 - 2 _ 2009",
             "1 _ 2009 - 3 _ 2009",
             "12 _ 2009 - 2 _ 2010",
             "1 _ 2010 - 3 _ 2010",
             "1 _ 2010 - 4 _ 2010",
             "1 _ 2011 - 3 _ 2011",
             "2 _ 2017 - 4 _ 2017")

combo_3_dat = month_dat[(month_dat$combo %in% combo_3), ] #48 (8 NAs from JSATS)

# wasn't sure how else to do this
combo_3_pdo <- read.csv("data/combo_3_pdo.csv")
combo_3_dat <- merge(combo_3_dat, combo_3_pdo, by = "combo")


pdo_all <- rbind(combo_1_dat[,c(2,3,10)], combo_2_dat[,c(2,3,13)], combo_3_dat[,c(2,3,14)])

model_df_pdo <- merge(sd_meta[,-c(2,7,8)], pdo_all[,c(1,3)], by = "FishID", all.x = TRUE)

write.csv(model_df_pdo, "results/model_dat.csv")
