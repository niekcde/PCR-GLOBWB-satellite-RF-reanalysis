####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####
source('select_station_codes.R')
# set directories 
grdcDir <- '../../data/preprocess/grdc_discharge/'
stationInfo <- read.csv('../../data/stationLatLon.csv')
stationInfo <- stationInfo %>% filter(wmo_reg == 6)

source('0_preprocess_grdc/fun_2_calculate_missing.R')
missing_list <- lapply(1:nrow(stationInfo), calculate_missing)

missing_col <- do.call(rbind,missing_list) 
colnames(missing_col) <- 'miss'
summary(missing_col)

stationInfo <- cbind(stationInfo,missing_col) 

write.csv(stationInfo, '../../data/stationLatLon.csv', row.names=F)


print(paste0('Number of stations with completely missing observations: ', sum(missing_col == 100))

