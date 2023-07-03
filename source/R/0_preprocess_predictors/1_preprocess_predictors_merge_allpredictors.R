####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####

stationInfo <- read.csv(paste0('../../data/stationLatLon.csv'))
stationInfo <- stationInfo %>% filter(grdc_no != 6233528) # not present in catchment attribute data

filePathCatchAttr <- paste0('../../data/predictors/pcr_parameters/')
filePathStatevars <- paste0('../../data/predictors/pcr_qMeteoStatevars/')
filePathSatelVars <- paste0('../../data/predictors/satellite/')

outputDir <- paste0('../../data/predictors/pcr_allpredictors/')
dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
file.remove(list.files(outputDir, full.names = TRUE))


add_lagg <- function(df,var ,months){
  index <- which(names(df) == var)
  lagged <- df %>%dplyr::mutate(new = lag(.[[index]], n = months, default = NA))
  lagged[paste0('lag_',var,'_',months)] <- lagged$new 
  lagged <- lagged %>% select(-new)
  return(lagged)
}

### function to merge tables of time-variant and statics predictors
merge_predictors <- function(i){
  
  station_no <- i
  # print(station_no)
	
  CatchAttrTable <- read.csv(paste0(filePathCatchAttr , 'pcr_parameters_',station_no,'.csv'))
  statevarsTable <- read.csv(paste0(filePathStatevars , 'pcr_qMeteoStatevars_',station_no,'.csv'))
  satelliteTable <- read.csv(paste0(filePathSatelVars , 'sat_predictors_', station_no, '.csv'))
  
  df_list = list(CatchAttrTable,
                 statevarsTable,
                 satelliteTable)
  
  allPredictors <- df_list %>% reduce(merge, by='datetime') %>%
    mutate(datetime = as.Date(datetime))
  
  lagged_months <- 12
  for (v in c('precipitation', 'temperature', 'referencePotET', 'lwe', 'sc', 'sm')){
    for (m in 1:lagged_months){
      allPredictors <- add_lagg(allPredictors, v, m)
    }
  }
    
  write.csv(allPredictors, paste0(outputDir, 'pcr_allpredictors_',station_no,'.csv'), row.names=FALSE)
  print(paste0('Progress: ', progress,'%'))

  
}
mclapply(stationInfo$grdc_no, merge_predictors, mc.cores=cores)



