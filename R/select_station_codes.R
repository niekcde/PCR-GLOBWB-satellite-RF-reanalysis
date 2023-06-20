# Station selection

select_stations <- function(){
  
  stationInfo <- read.csv('../../data/stationLatLon_catchAttr.csv')
  stationInfo <- stationInfo %>% filter(wmo_reg == 6) %>%  select(grdc_no)
  return(stationInfo$grdc_no)
}


