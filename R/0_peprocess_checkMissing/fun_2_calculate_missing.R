#### processing ####
calculate_missing <- function(i, start_date, end_date){ 
  
  station_no <- i
  
  grdDir <- '../../data/preprocess/grdc_discharge/grdc_'
  satDir <- paste0('../../data/predictors/satellite_all/sat_predictors_')
  
  grdFile <- paste0(grdDir, station_no,'.csv')
  satFile <- paste0(satDir, station_no,'.csv')
  
  fully_missing <- c()
  if(file.exists(grdFile) & file.exists(satFile)){
    grd <- read.csv(grdFile)
    sat <- read.csv(satFile)
    
    obs <- inner_join(grd, sat, by = 'datetime')
    
    obs <- obs %>% filter(datetime >= start_date & datetime <= end_date) 
    
    missings <- apply(obs, 2, function(x) sum(is.na(x)))[2:5]
    missing_perc <- round((missings / nrow(obs)) * 100, 2)
    
    
    if (any(missing_perc == 100)){
      fully_missing <-  as.integer(station_no)
    }else{fully_missing <-  0}
    
    if (nrow(drop_na(obs)) == 0){
      fully_missing <- as.integer(station_no)
    }
    
  }else{
    print(paste0(station_no, ': sat or grdc doesnt exist'))
    missing_perc <- 100
  }

  return(c(missing_perc, fully_missing))
}
