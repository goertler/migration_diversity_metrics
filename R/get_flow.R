# add Sacramento River dayflow
# add inundation data

# library
library(devtools)

devtools::install_github("goertler/inundation")

library(inundation)

inun <- calc_inundation()

head(inun)
str(inun)

write.csv(inun[,c(1,2,5)], "data/flow_dat.csv")
