####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####
all_dir <- '_all'
# all_dir <- ''
# lagged <- 'lagged_'
lagged <- ''

stationInfo <- read.csv(paste0('../../data/stationLatLon.csv'))

filePathCatchAttr <- paste0('../../data/predictors/pcr_parameters/')
filePathStatevars <- paste0('../../data/predictors/pcr_qMeteoStatevars/')
filePathSatelVars <- paste0('../../data/predictors/satellite',all_dir,'/')

outputDir <- paste0('../../data/predictors/pcr_allpredictors',all_dir,'/')
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
  satelliteTable <- read.csv(paste0(filePathSatelVars, 'sat_predictors_',lagged, station_no, '.csv'))
  
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
    
  write.csv(allPredictors, paste0(outputDir, 'pcr_allpredictors_', lagged,station_no,'.csv'), row.names=FALSE)
  print(paste0('Progress: ', progress,'%'))

  
}
source('select_station_codes.R')
# mclapply(stationInfo$grdc_no, merge_predictors, mc.cores=24)
count<-1
progress <- 0
for (i in stationInfo$grdc_no){
  if (i != 6233528){
    merge_predictors(i)
    
  }else{print(i)}
  count <- count + 1
  progress <- (count / nrow(stationInfo)) * 100 
  
}


