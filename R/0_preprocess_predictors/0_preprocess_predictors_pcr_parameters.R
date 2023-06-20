####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####

#### set-up ####
stationInfo <- read.csv('../../data/stationLatLon_catchAttr.csv')
outputDir <- '../../data/predictors/pcr_parameters/'
dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
  
# datetime as pcr-globwb run
startDate <- '1979-01-01'
endDate <- '2019-12-31'
dates <- as.data.frame(seq(as.Date(startDate), as.Date(endDate), by="month"))
colnames(dates) <- 'datetime'

#### run ####
source('0_preprocess_predictors/fun_0_preprocess_pcr_parameters.R')
source('select_station_codes.R')
# mclapply(select_stations(), create_predictor_table, mc.cores=24)

mclapply(stationInfo$grdc_no, create_predictor_table, mc.cores=24)

