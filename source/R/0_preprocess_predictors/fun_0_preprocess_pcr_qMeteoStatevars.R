create_predictor_table <- function(i){
  
  station_no <- i
  upstreamArea <- stationInfo %>% filter(grdc_no == station_no) %>% select(area)

  ####-------discharge-------####
  obs <- read.csv(paste0(filePathGrdc, 'grdc_', station_no, '.csv')) %>% 
    filter(datetime %in% as.character(dates)) %>% 
    mutate(datetime=dates) 
  pcr <- read.csv(paste0(filePathDischarge, 'pcr_discharge_', station_no, '.csv')) %>%
    filter(datetime %in% as.character(dates)) %>% 
    mutate(datetime=as.Date(datetime))
  pred <- read.csv(paste0(filePathStatevars, 'pcr_statevars_',station_no,'.csv')) %>%
    filter(datetime %in% as.character(dates)) %>% 
    mutate(datetime=as.Date(datetime)) %>% 
    select(-c('channelStorage', 'totLandSurfaceActuaET')) #related to other statevars
  
  # join obs pcr discharge in dataframe and normalize to area 
  # and converted to daily discharge instead of discharge per second
  q <- inner_join(obs, pcr, by='datetime') %>% mutate(obs = obs / upstreamArea$area*0.0864, 
                                                      pcr = pcr / upstreamArea$area*0.0864)
  
  
  ####-------normalize statevars [-1 1] and join to q-------####
  pred_norm <- pred %>% mutate(across(!datetime, scale)) # z-score normalization
  pred_norm[is.na(pred_norm)] <- 0
  
  pred_table <- inner_join(q, pred_norm, by='datetime')
  
  write.csv(pred_table, paste0(outputDir, 'pcr_qMeteoStatevars_',
                               station_no, '.csv'), row.names = F)
}
