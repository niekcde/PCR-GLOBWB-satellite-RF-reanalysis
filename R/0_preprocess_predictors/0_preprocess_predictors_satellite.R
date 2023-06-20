####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####

outputDir <- '../../data/predictors/satellite_all/'
dir.create(outputDir, showWarnings = F, recursive = T)
file.remove(list.files(outputDir, full.names = TRUE))

combine_sattelite <- function(station_no){
  
  start <- as.Date('2002-04-01')
  end <- as.Date('2019-12-01')
  dates <- tibble(datetime = as.character(seq(start, end, by ='1 month')))
  
  read_satellite_data <- function(sat, var){
    data <- read.csv(paste0('../../data/satellite_data/',sat,'/upstream_station_all/',station_no,'_',var,'.csv')) 
    data <- left_join(dates, data, by ='datetime')
    return(data)
  }
  
  sm            <- read_satellite_data('ESA', 'sm')
  lwe_thickness <- read_satellite_data('GRACE','lwe')
  snow_cover    <- read_satellite_data('MODIS' , 'sc')
  
  df_list = list(sm, lwe_thickness, snow_cover) 
  combined <- df_list %>% reduce(merge, by='datetime')
  
  write.csv(combined, paste0(outputDir, 'sat_predictors_',station_no,'.csv'), row.names = F)
}

source('select_station_codes.R')
stations <- read.csv('../../data/stationLatLon.csv')

mclapply(stations$grdc_no, combine_sattelite)



