####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####

filePath <- '../../data/predictors/pcr_allpredictors_all/pcr_allpredictors_1134300.csv'
variables <- tibble(names = names(read.csv(filePath)))



lagged_sat   <- variables$names[grep(pattern = '.*lag_lwe.*|.*lag_sc.*', variables$names)]
lagged_meteo <- variables$names[grep(pattern = '.*lag_ref.*|.*lag_prec.*|.*lag_temp.*', variables$names)]

lagged_4sm     <- variables$names[grep(pattern = 'lag.*_[0-4]$', variables$names)]
lagged_6sm     <- variables$names[grep(pattern = 'lag.*_[0-6]$', variables$names)]
lagged_8sm     <- variables$names[grep(pattern = 'lag.*_[0-8]$', variables$names)]
lagged_12sm    <- variables$names[grep(pattern = 'lag.*_[0-9]', variables$names)]

lagged_4     <- variables$names[grep(pattern = 
                                       'lag_lwe_[0-4]$|lag_sc_[0-4]$|lag_ref.*_[0-4]$|lag_prec.*_[0-4]$|lag_temp.*_[0-4]$',
                                       variables$names)]
lagged_6     <- variables$names[grep(pattern = 
                                       'lag_lwe_[0-6]$|lag_sc_[0-6]$|lag_ref.*_[0-6]$|lag_prec.*_[0-6]$|lag_temp.*_[0-6]$',
                                     variables$names)]
lagged_8     <- variables$names[grep(pattern = 
                                       'lag_lwe_[0-8]$|lag_sc_[0-8]$|lag_ref.*_[0-8]$|lag_prec.*_[0-8]$|lag_temp.*_[0-8]$',
                                     variables$names)]
lagged_12    <- variables$names[grep(pattern = 
                                       'lag_lwe_[0-9]|lag_sc_[0-9]|lag_ref.*_[0-9]|lag_prec.*_[0-9]|lag_temp.*_[0-9]',
                                     variables$names)]
lagged_sat4  <- variables$names[grep(pattern = 'lag_lwe_[0-4]$|lag_sc_[0-4]$', variables$names)]
lagged_sat6  <- variables$names[grep(pattern = 'lag_lwe_[0-8]$|lag_sc_[0-8]$', variables$names)]
lagged_sat8  <- variables$names[grep(pattern = 'lag_lwe_[0-6]$|lag_sc_[0-6]$', variables$names)]
lagged_sat12 <- variables$names[grep(pattern = 'lag_lwe_[0-9]|lag_sc_[0-9]', variables$names)]

lagged_satsm4  <- variables$names[grep(pattern = 'lag_lwe_[0-4]$|lag_sc_[0-4]$|lag_sm_[0-4]$', variables$names)]
lagged_satsm6  <- variables$names[grep(pattern = 'lag_lwe_[0-8]$|lag_sc_[0-8]$|lag_sm_[0-8]$', variables$names)]
lagged_satsm8  <- variables$names[grep(pattern = 'lag_lwe_[0-6]$|lag_sc_[0-6]$|lag_sm_[0-6]$', variables$names)]
lagged_satsm12 <- variables$names[grep(pattern = 'lag_lwe_[0-9]|lag_sc_[0-9]|lag_sm_[0-9]', variables$names)]

sat          <- variables$names[grep(pattern = 'lwe$|sc$|sm$', variables$names)]
pcr          <- variables$names[!variables$names %in% c(lagged_12sm, sat)]


predictors_pcr <- variables %>% filter(!names %in% c('datetime'), names %in% pcr)
predictors_pcr_sat <- variables %>% filter(!names %in% c('datetime','snowCoverSWE', 'storUppTotal', 'storGroundwater',
                                                         'storLowTotal', lagged_12sm))


predictors_pcr_sat_add <- variables %>% filter(!names %in% c('datetime', lagged_12sm))

sat_meteo_predictors_static <- sat_meteo_predictors <- variables %>% 
  filter(names %in% c('obs', 'sm', 'lwe','sc', 
                      'precipitation', 'temperature', 'referencePotET', static))

write.csv(predictors_pcr_sat_add, '../../data/predictors_pcr_sat_add.csv',row.names = F)
write.csv(sat_meteo_predictors_static, '../../data/predictors_sat_meteo_static.csv',row.names = F)

# pcr sat lagged ########
predictors_pcr_sat_lagged4 <- variables %>% filter(names %in% c(pcr, lagged_sat4),
                                                   !names %in% c('datetime','snowCoverSWE', 'storUppTotal', 'storGroundwater'
                                                                 , 'storLowTotal'))
predictors_pcr_sat_lagged6 <- variables %>% filter(names %in% c(pcr, lagged_sat6),
                                                   !names %in% c('datetime','snowCoverSWE', 'storUppTotal', 'storGroundwater'
                                                                 , 'storLowTotal'))
predictors_pcr_sat_lagged8 <- variables %>% filter(names %in% c(pcr, lagged_sat8),
                                                   !names %in% c('datetime','snowCoverSWE', 'storUppTotal', 'storGroundwater'
                                                                 , 'storLowTotal'))
predictors_pcr_sat_lagged12 <- variables %>% filter(names %in% c(pcr, lagged_sat12),
                                                    !names %in% c('datetime','snowCoverSWE', 'storUppTotal', 'storGroundwater'
                                                                  , 'storLowTotal'))
# pcr sat lagged with sm lag #####
predictors_pcr_sat_laggedsm4 <- variables %>% filter(names %in% c(pcr, lagged_satsm4),
                                                   !names %in% c('datetime','snowCoverSWE', 'storUppTotal', 'storGroundwater'
                                                                 , 'storLowTotal'))
predictors_pcr_sat_laggedsm6 <- variables %>% filter(names %in% c(pcr, lagged_satsm6),
                                                   !names %in% c('datetime','snowCoverSWE', 'storUppTotal', 'storGroundwater'
                                                                 , 'storLowTotal'))
predictors_pcr_sat_laggedsm8 <- variables %>% filter(names %in% c(pcr, lagged_satsm8),
                                                   !names %in% c('datetime','snowCoverSWE', 'storUppTotal', 'storGroundwater'
                                                                 , 'storLowTotal'))
predictors_pcr_sat_laggedsm12 <- variables %>% filter(names %in% c(pcr, lagged_satsm12),
                                                    !names %in% c('datetime','snowCoverSWE', 'storUppTotal', 'storGroundwater'
                                                                  , 'storLowTotal'))

# sat meteo lag ######

sat_meteo_predictors <- variables %>% filter(names %in% c('obs', 'sm', 'lwe','sc', 'precipitation', 'temperature'
                                                          , 'referencePotET'))
sat_meteo_lagged_predictors4 <- variables %>% filter(names %in% c('obs','precipitation', 'temperature'
                                                                 , 'referencePotET',sat, lagged_4))
sat_meteo_lagged_predictors6 <- variables %>% filter(names %in% c('obs','precipitation', 'temperature'
                                                                  , 'referencePotET',sat, lagged_6))
sat_meteo_lagged_predictors8 <- variables %>% filter(names %in% c('obs','precipitation', 'temperature'
                                                                 , 'referencePotET',sat, lagged_8))
sat_meteo_lagged_predictors12 <- variables %>% filter(names %in% c('obs','precipitation', 'temperature'
                                                                 , 'referencePotET',sat, lagged_12))


# sat meteo lag with sm #####

sat_meteo_lagged_predictorssm4 <- variables %>% filter(names %in% c('obs','precipitation', 'temperature'
                                                                  , 'referencePotET',sat, lagged_4sm))
sat_meteo_lagged_predictorssm6 <- variables %>% filter(names %in% c('obs','precipitation', 'temperature'
                                                                  , 'referencePotET',sat, lagged_6sm))
sat_meteo_lagged_predictorssm8 <- variables %>% filter(names %in% c('obs','precipitation', 'temperature'
                                                                  , 'referencePotET',sat, lagged_8sm))
sat_meteo_lagged_predictorssm12 <- variables %>% filter(names %in% c('obs','precipitation', 'temperature'
                                                                   , 'referencePotET',sat, lagged_12sm))

# write files ######
write.csv(predictors_pcr, '../../data/predictors_pcr.csv',row.names = F)

write.csv(predictors_pcr_sat, '../../data/predictors_pcr_sat.csv',row.names = F)
write.csv(predictors_pcr_sat_lagged4, '../../data/predictors_pcr_sat_lagged_4.csv',row.names = F)
write.csv(predictors_pcr_sat_lagged6, '../../data/predictors_pcr_sat_lagged_6.csv',row.names = F)
write.csv(predictors_pcr_sat_lagged8, '../../data/predictors_pcr_sat_lagged_8.csv',row.names = F)
write.csv(predictors_pcr_sat_lagged12, '../../data/predictors_pcr_sat_lagged_12.csv',row.names = F)

write.csv(sat_meteo_predictors, '../../data/predictors_sat_meteo.csv',row.names = F)
write.csv(sat_meteo_lagged_predictors4, '../../data/predictors_sat_meteo_lagged_4.csv',row.names = F)
write.csv(sat_meteo_lagged_predictors6, '../../data/predictors_sat_meteo_lagged_6.csv',row.names = F)
write.csv(sat_meteo_lagged_predictors8, '../../data/predictors_sat_meteo_lagged_8.csv',row.names = F)
write.csv(sat_meteo_lagged_predictors12, '../../data/predictors_sat_meteo_lagged_12.csv',row.names = F)


write.csv(predictors_pcr_sat_laggedsm4, '../../data/predictors_pcr_sat_lagged_4_sm.csv',row.names = F)
write.csv(predictors_pcr_sat_laggedsm6, '../../data/predictors_pcr_sat_lagged_6_sm.csv',row.names = F)
write.csv(predictors_pcr_sat_laggedsm8, '../../data/predictors_pcr_sat_lagged_8_sm.csv',row.names = F)
write.csv(predictors_pcr_sat_laggedsm12, '../../data/predictors_pcr_sat_lagged_12_sm.csv',row.names = F)

write.csv(sat_meteo_lagged_predictorssm4, '../../data/predictors_sat_meteo_lagged_4_sm.csv',row.names = F)
write.csv(sat_meteo_lagged_predictorssm6, '../../data/predictors_sat_meteo_lagged_6_sm.csv',row.names = F)
write.csv(sat_meteo_lagged_predictorssm8, '../../data/predictors_sat_meteo_lagged_8_sm.csv',row.names = F)
write.csv(sat_meteo_lagged_predictorssm12, '../../data/predictors_sat_meteo_lagged_12_sm.csv',row.names = F)





static <- c('airEntry1', 
'airEntry2' ,
'aqThick' ,
'aridityIdx',
'bankArea' ,
'bankDepth' ,
'bankWidth' ,
'demAverage',
'forestFraction' ,
'groundwaterDepth',
'KSat1' ,
'KSat2' ,
'kSatAquifer', 
'recessionCoeff',
'resWC1' ,
'resWC2' ,
'satWC1' ,
'satWC2' ,
'slopeLength', 
'specificYield',
'storage1' ,
'storage2' ,
'storDepth1', 
'storDepth2' ,
'tanSlope' ,
'catchment',
'poreSize1',
'poreSize2',
'percolationImp')










