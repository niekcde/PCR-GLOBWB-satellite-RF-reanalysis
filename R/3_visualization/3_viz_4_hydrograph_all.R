####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####
source('3_visualization/fun_3_4_hydrograph_fdc_residuals_all.R')
####-------------------------------####

## plot observed, uncalibrated, corrected hydrograph
#~ subsample <- '1' # choose subsample number here
combiPlot <- function(i){  
  
  station_no <- stationInfo$grdc_no[i]
  upstreamArea <- stationInfo$area[i]
  convRatio <- upstreamArea/0.0864
  
  print(station_no)
  if (country == 'all'){
    setup <- c('pcr', 'pcr_sat', 'sat_meteo', 'sat_meteo_static')
  }else{
    setup <- c('pcr', 'pcr_sat', 'sat_meteo_lagged_4')
  }
  # station_no <- 6140400
  KGE_string <- ''
  for (s in 1:length(setup)){
    KGE_table <- read.csv(paste0('../../RF/3_validate/subsample_',subsample,'_',country,'/KGE_predictors_',setup[s],'.csv')) %>%
      inner_join(.,read.csv(paste0('../../data/stationLatLon_selected_',country,'.csv')) %>% 
                   select(c('grdc_no','miss_obs')), by='grdc_no') %>% mutate(miss = miss_obs)
    
    KGE_corrected <- round(KGE_table$KGE_corrected[KGE_table$grdc_no==station_no],2)
    KGE_string <- paste0(KGE_string,', ', KGE_corrected, ' (', setup[s], ')')
  }
  KGE_string <- paste0('KGE: ', sub(',','',KGE_string))
  # KGE_table <- read.csv(paste0('../../RF/3_validate/subsample_',subsample,'_',country,'/KGE_predictors_pcr.csv')) %>%
  #   inner_join(.,read.csv(paste0('../../data/stationLatLon_selected_',country,'.csv')) %>% 
  #                select(c('grdc_no','miss_obs')), by='grdc_no') %>% mutate(miss = miss_obs)
  
  # KGE_uncalib <- round(KGE_table$KGE[KGE_table$grdc_no==station_no],2)
  # KGE_corrected <- round(KGE_table$KGE_corrected[KGE_table$grdc_no==station_no],2)
  miss_data <- KGE_table$miss[KGE_table$grdc_no==station_no]
  
  
  hg <- plot_hydrograph(station_no, convRatio, setup)
  fdc <- plot_ecdf(station_no, convRatio, setup)
  resPlot <- plot_residuals(station_no, convRatio, setup)
  
  
  combined <- hg / fdc  / resPlot 
  combined <- combined + 
    plot_annotation(
      title = paste0(station_no,' : ', str_to_title(stationInfo$station[i]), ' (',
                      str_to_title(stationInfo$river[i]), ', ', stationInfo$country[i], ')'),
      subtitle = paste0('lat: ', stationInfo$lat[i], ', lon: ', stationInfo$lon[i],
                        '\nUpstream area: ', upstreamArea, ' km2'),
      caption = paste0(KGE_string, '\n',
                       'Missing data (2002-2019): ', miss_data, '%')) +
    plot_layout(guides='collect') &
    theme(plot.title = element_text(hjust= 0.5, size = 22, face='bold'),
          plot.subtitle = element_text(hjust= 0.5, size = 18),
          plot.caption = element_text(size = 16),
          text = element_text('mono'),
          legend.position = 'bottom')
  # combined
  
  ggsave(paste0(outputDirCombo,'comboPlot_',station_no,'.png'), combined, height=10, width=20, units='in', dpi=300)
  
}

country <- 'AU'
country <- 'all'
country <- 'CA'
# country <- 'US'


#### all subsamples ####
for(subsample in 1:5){
    
    print(paste0('subsample: ', subsample))
    
    dir <- paste0('../../RF/3_validate/subsample_', subsample,'_',country, '/tables_pcr_sat_predictors/')

    stationInfo <- read.csv(paste0('../../RF/0_rf_input/subsample_',subsample,'_',country,'/test_stations.csv'))

    KGE_table <- read.csv(paste0('../../RF/3_validate/subsample_',subsample,'_',country,'/KGE_predictors_pcr.csv')) %>%
      inner_join(.,read.csv(paste0('../../data/stationLatLon_selected_',country,'.csv')) %>% 
                   select(c('grdc_no','miss_obs')), by='grdc_no') %>% mutate(miss = miss_obs)

    #### a combined plot test ####
    outputDirCombo <- paste0('../../viz/comboPlots/subsample_', subsample,'_',country, '/')
    dir.create(outputDirCombo, showWarnings = F, recursive = T)
    print('plotting...')

    for (s in 1:nrow(stationInfo)){
      combiPlot(s)
    }
    # mclapply(1:nrow(stationInfo), combiPlot, mc.cores=4)
}
