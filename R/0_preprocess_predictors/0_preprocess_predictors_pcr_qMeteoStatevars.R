####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####

stationInfo <- read.csv('../../data/stationLatLon.csv')

#grdc stations full time series
filePathGrdc <- paste0('../../data/preprocess/grdc_discharge/')
#pcr-globwb time series 1979-2019
filePathDischarge <- paste0('../../data/preprocess/pcr_discharge/')
filePathStatevars <- paste0('../../data/preprocess/pcr_statevars/')

outputDir <- '../../data/predictors/pcr_qMeteoStatevars/'
dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)

# datetime as pcr-globwb run
startDate <- '1979-01-01'
endDate <- '2019-12-31'
dates <- seq(as.Date(startDate), as.Date(endDate), by="month")

source('0_preprocess_predictors/fun_0_preprocess_pcr_qMeteoStatevars.R')
source('select_station_codes.R')
mclapply(select_stations(), create_predictor_table, mc.cores=48)