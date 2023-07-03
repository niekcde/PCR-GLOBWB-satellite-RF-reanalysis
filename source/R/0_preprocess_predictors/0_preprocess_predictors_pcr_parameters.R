####-------------------------------####
source('fun_0_loadLibrary.R')
source('project_settings.R')
####-------------------------------####

#### set-up ####
stationInfo <- read.csv('../../data/stationLatLon_catchAttr.csv') # catchment attribute file for all stations
outputDir   <- '../../data/predictors/pcr_parameters/'
dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
  
# datetime as pcr-globwb run
dates <- as.data.frame(seq(as.Date(startDate), as.Date(endDate), by="month"))
colnames(dates) <- 'datetime'

#### run ####
source('0_preprocess_predictors/fun_0_preprocess_pcr_parameters.R')
mclapply(stationInfo$grdc_no, create_predictor_table, mc.cores=cores)

