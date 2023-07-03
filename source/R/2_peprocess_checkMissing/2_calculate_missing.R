####-------------------------------####
source('fun_0_loadLibrary.R')
source('project_settings.R')
####-------------------------------####

remove_missings <- function(file, write_file){
  stationInfo <- read.csv(paste0('../../data/',file))
  
  source('2_peprocess_checkMissing/fun_2_calculate_missing.R')
  missing_list <- lapply(stationInfo$grdc_no, calculate_missing)

  missing <- as.data.frame(do.call(rbind,missing_list))
 
  # select station with 100% missing values in any of the observational columns
  missing_stations <- missing %>% filter(if_any(-grdc_no, any_vars(.==100)))
    

  print(summary(missing %>% select(-grdc_no)))
  
  missing_cols <- c('miss_obs', paste0('miss_',satelliteProducts))
  missing_cols <- c('obs', satelliteProducts)
  for (col in missing_cols){
    if (col %in% names(stationInfo)){
      stationInfo <- stationInfo %>% select(-contains(col))
    }
  }
  stationInfo <- stationInfo %>% inner_join(missing, by = 'grdc_no') %>% 
    filter(if_all(missing_cols, all_vars(.!=100)))
  
  
  stationInfo %>% setnames(old = missing_cols, new = paste0('miss_',missing_cols))

  #remove stations not in the pcr_parameters files
  stations_pcr <- read.csv('../../data/stationLatLon_catchAttr.csv')$grdc_no
  not_in_pcr_parameter <- stationInfo$grdc_no[!stationInfo$grdc_no %in% stations_pcr]
  stationInfo <- stationInfo %>% filter(!grdc_no %in% not_in_pcr_parameter)

  print(paste0('Number of removed stations: ', nrow(missing_stations)))
  print(paste0('Number of available stations: ', nrow(stationInfo)))
  if (write_file == T){
  write.csv(stationInfo, paste0('../../data/',file), row.names=F)
  }
}


filePathRF <- paste0('../../data/')
file_list <- list.files(filePathRF, pattern = 'selected_')

for(file in file_list){
  print(file)
  remove_missings(file, T)
}





