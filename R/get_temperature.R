# library
library(contentid)

# temperature at Rio Vista
data_URL = "https://portal.edirepository.org/nis/dataviewer?packageid=edi.1178.1&entityid=5055c89851653f175078378a6e8ba6eb"
integrated_data_id <- contentid::store(data_URL)
integrated_temp <- read.csv(contentid::retrieve(integrated_data_id))
str(integrated_temp)
integrated_temp$date <- as.Date(integrated_temp$date)

temp_daily <- subset(integrated_temp, region == "river_downstream")

head(temp_daily)

write.csv(temp_daily[,c(1,2)], "data/temperature_dat.csv")
