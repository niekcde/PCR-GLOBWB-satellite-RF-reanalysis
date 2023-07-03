####-------------------------------####
source('fun_0_loadLibrary.R')
source('project_settings.R')
####-------------------------------####

outputDir <- '../../data/predictors/satellite/'
dir.create(outputDir, showWarnings = F, recursive = T)
file.remove(list.files(outputDir, full.names = TRUE))

combine_sattelite <- function(station_no){
  
  
  dates <- tibble(datetime = as.character(seq(as.Date(startDate), as.Date(endDate), by ='1 month')))
  
  satList <- list()
  read_satellite_data <- function(var){
    data <- read.csv(paste0('../../data/satellite_data/',var,'/upstream_station/',station_no,'_',var,'.csv')) 
    data <- left_join(dates, data, by ='datetime') %>% rename(sat = var) %>% mutate(satName = var)
    return(data)
  }
  for (s in 1:length(satelliteProducts)){
    satList[[s]] <- read_satellite_data(satelliteProducts[s])
  }
  sat <- do.call(rbind, satList)
  sat <- pivot_wider(sat, names_from = satName, values_from = sat)
  
  write.csv(sat, paste0(outputDir, 'sat_predictors_',station_no,'.csv'), row.names = F)
}

stations <- read.csv('../../data/stationLatLon.csv')
mclapply(stations$grdc_no, combine_sattelite)



