create_predictor_table <- function(i){
  
  station_no <- i
  upstreamArea <- stationInfo %>% filter(grdc_no == station_no) %>% select(area)

  ####-------discharge-------####
  obs <- read.csv(paste0(filePathGrdc, 'grdc_', station_no, '.csv')) %>% 
    mutate(datetime=dates) 
  pcr <- read.csv(paste0(filePathDischarge, 'pcr_discharge_', station_no, '.csv')) %>%
    mutate(datetime=as.Date(datetime))
  pred <- read.csv(paste0(filePathStatevars, 'pcr_statevars_',station_no,'.csv')) %>%
    mutate(datetime=as.Date(datetime)) %>% 
    select(-c('channelStorage', 'totLandSurfaceActuaET')) 
   
#~   #upscale2monthly
#~   pcr_monthly <- pcr %>% mutate(datetime = floor_date(datetime, 'month')) %>% 
#~     group_by(datetime) %>% 
#~     summarise(.,pcr=mean(pcr))
#~   pred_monthly <- pred %>% mutate(datetime = floor_date(datetime, 'month')) %>% 
#~     group_by(datetime) %>% 
#~     summarise_all(., ~mean(.))
  
  # join obs pcr discharge in dataframe and normalize to area 
  # and converted to daily discharge instead of discharge per second
  q <- inner_join(obs, pcr, by='datetime')
  q <- ((q[,-1])/upstreamArea$area*0.0864) %>%
    cbind(datetime=q$datetime, .)
  
  ####-------normalize statevars [-1 1] and join to q-------####
  
  pred_norm <- pred %>% select(-datetime)
  pred_norm <- scale(pred_norm) %>% # z-score normalization
    cbind(pred %>% select(datetime),.) %>%
    mutate(datetime=as.Date(datetime))
  pred_norm[is.na(pred_norm)] <- 0
  
  pred_table <- inner_join(q, pred_norm, by='datetime')
  
  write.csv(pred_table, paste0(outputDir, 'pcr_qMeteoStatevars_',
                               station_no, '.csv'), row.names = F)
}
