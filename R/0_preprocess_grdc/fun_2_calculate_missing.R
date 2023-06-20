#### processing ####
calculate_missing <- function(i){ 
  
  station_no <- stationInfo$grdc_no[i]
  # print(station_no)
  
  filename <- paste0(grdcDir,'grdc_', station_no,'.csv')
  fully_missing <- c()
  if(file.exists(filename)){
    grdc <- read.csv(filename)
    grdc <- grdc %>%filter(datetime >= '2002-04-01' & datetime <= '2019-12-01')
    # calculate missing percentage
    missing_perc <- round((sum(is.na(grdc$obs)) / nrow(grdc) * 100), 2)
    
    if (missing_perc == 100){
      print(station_no)
      fully_missing <- c(fully_missing, station_no)
    }
    
  }else{
    print(paste0(station_no, ' :doesnt exist'))
    missing_perc <- 100
    }
  
  return(missing_perc)
}