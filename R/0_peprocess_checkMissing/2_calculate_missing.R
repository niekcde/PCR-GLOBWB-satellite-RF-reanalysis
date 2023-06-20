####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####
source('select_station_codes.R')

remove_missings <- function(stationInfo, filter_stations, all_dir,name, write_file){
  # open en select correct stations
  stationInfo <- read.csv('../../data/stationLatLon.csv')
  
  # filter_stations <- USA$grdc_no
  # filter_stations <- australie$grdc_no
  # filter_stations <- canada$grdc_no
  # filter_stations <- bad$grdc_no

  if (length(filter_stations) != 0){
    stationInfo <- stationInfo %>% filter(grdc_no %in% filter_stations)
  }
  start_date <- '2002-04-01'
  end_date <- '2019-12-01'
  
  # set directories 
  grdcDir <- '../../data/preprocess/grdc_discharge/grdc_'
  sat <- paste0('../../data/predictors/satellite',all_dir,'/sat_predictors_')
  
  source('0_peprocess_checkMissing/fun_2_calculate_missing.R')
  missing_list <- lapply(stationInfo$grdc_no, calculate_missing, start_date, end_date)

  missing <- tibble(miss_obs = do.call(rbind,missing_list)[,1],
                    miss_sm = do.call(rbind,missing_list)[,2],
                    miss_lwe = do.call(rbind,missing_list)[,3],
                    miss_sc = do.call(rbind,missing_list)[,4],
                    station = do.call(rbind,missing_list)[,5])

  missing_stations <- missing %>% filter(station != 0) %>% select(station) %>% 
    mutate(station = as.integer(station))
  

  print(summary(missing))
  
  for (col in c('miss_obs', 'miss_sm', 'miss_sc', 'miss_lwe')){
    if (col %in% names(stationInfo)){
      stationInfo <- stationInfo %>% select(-contains(col))
    }
  }
  stationInfo <- cbind(stationInfo,missing[, 1:4])
  if (write_file == T){
    write.csv(stationInfo, paste0('../../data/stationLatLon',name,'_with_missing.csv'), row.names=F)
  }
  #remove stations for RF runs
  stationInfo <- stationInfo %>% filter(!grdc_no %in% missing_stations$station)

  #remove stations not in the pcr_parameters files
  stations_pcr <- read.csv('../../data/stationLatLon_catchAttr.csv')$grdc_no
  not_in_pcr_parameter <- stationInfo$grdc_no[!stationInfo$grdc_no %in% stations_pcr]
  stationInfo <- stationInfo %>% filter(!grdc_no %in% not_in_pcr_parameter)

  print(paste0('Number of removed stations: ', nrow(missing_stations)))
  print(paste0('Number of available stations: ', nrow(stationInfo)))
  if (write_file == T){
  write.csv(stationInfo, paste0('../../data/stationLatLon',name,'.csv'), row.names=F)
  }
}
stationInfo <- read.csv('../../data/stationLatLon.csv')

canada    <- stationInfo %>% filter(country == 'CA')
australie <- stationInfo %>% filter(country == 'AU')
USA       <- stationInfo %>% filter(country == 'US') 

remove_missings(stationInfo, canada$grdc_no, '_all', '_selected_CA'   , T)
remove_missings(stationInfo, australie$grdc_no, '_all', '_selected_AU', T)
remove_missings(stationInfo, USA$grdc_no, '_all', '_selected_US'      , T)
remove_missings(stationInfo, c(), '_all', '_selected_all', T)




