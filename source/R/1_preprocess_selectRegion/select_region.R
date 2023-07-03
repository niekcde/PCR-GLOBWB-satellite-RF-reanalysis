# select region if different runs besides global are needed

stationInfo <- read.csv(paste0('../../data/stationLatLon.csv'))

canada    <- stationInfo %>% filter(country == 'CA')
write.csv(canada, paste0('../../data/stationLatLon_selected_CA.csv'), row.names=F)

australia <- stationInfo %>% filter(country == 'AU')
write.csv(australia, paste0('../../data/stationLatLon_selected_AU.csv'), row.names=F)

USA       <- stationInfo %>% filter(country == 'US') 
write.csv(USA, paste0('../../data/stationLatLon_selected_US.csv'), row.names=F)

global    <- stationInfo
write.csv(stationInfo, paste0('../../data/stationLatLon_selected_all.csv'), row.names=F)


