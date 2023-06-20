####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####

outputDir <- '../../viz/varImportance/'
dir.create(outputDir, showWarnings = F, recursive = T)

#choose runs
regions <- c('AU')
regions <- c('CA')
# regions <- c('US')
# regions <- c('all')
samples <- 5

filePathRF <- paste0('../../data/')
setup <- list.files(filePathRF, pattern = 'predictors_')
if (regions[1] =='all'){
  setup <- c('predictors_pcr.csv','predictors_pcr_sat.csv', 'predictors_pcr_sat_add.csv',
                     'predictors_sat_meteo.csv', 'predictors_sat_meteo_static.csv')
}else{
  setup <- c("predictors_pcr.csv", "predictors_pcr_sat.csv",
             "predictors_pcr_sat_lagged_4.csv",  "predictors_pcr_sat_lagged_12.csv",
             "predictors_sat_meteo.csv", "predictors_sat_meteo_lagged_4.csv", "predictors_sat_meteo_lagged_12.csv")
}

remove <- c('datetime', 'obs')
predNames <- read.csv('../../data/predictors/pcr_allpredictors_all/pcr_allpredictors_6123400.csv') %>% 
  select(-datetime, -obs) %>% names(.) 

setupPlotlist <- list()
#### 5 subsamples variable importance ####
for(i in 1:length(setup)){
  variables <- read.csv(paste0(filePathRF, setup[i]))
  setup_name <- str_sub(setup[i], end = -5)
  viPlotList <- list()
  count <- 1

  for (c in regions){
    viList <- list()
    
    for(subsample in 1:samples){
      
      trainDir <- paste0('../../RF/2_train/subsample_',subsample,'_',c,'/')
      
      if(subsample==1){
        viList[[subsample]] <- read.csv(paste0(trainDir, 'varImportance_',setup_name,'.csv')) %>% 
          rename(importance_1=importance)
      } else{
        viList[[subsample]] <- read.csv(paste0(trainDir, 'varImportance_',setup_name,'.csv')) %>%
          select(., importance) %>%  rename(!!paste0('importance_',subsample) := importance)
      }
    }
  
    viSetup <- as.data.frame(do.call(cbind, viList))
    
    # calculate avg and standard deviation of variable importances
    for(j in 1:nrow(viSetup)){
      viSetup$importance_avg[j] <- sum(viSetup[j,2:(samples +1)]) / 5
      viSetup$importance_sd[j] <- sd(viSetup[j,2:(samples +1)])
    }
    # rename pcr to pcrFlowDepth
    index <- which(viSetup$names %in% c('pcr'))
    viSetup[index,1]='pcrFlowDepth'

    #gather
    plotData <- viSetup %>% slice_max(n = 20, order_by= importance_avg) %>%
      select(names, importance_avg, importance_sd) %>% 
      pivot_longer(importance_avg, names_to = 'key', values_to = 'value')
    
    # add predictor type (static or time-variant) to color plot text
    plotData$predictorType <- 1
    lagged_meteo <- grep(pattern = '.*lag_ref.*|.*lag_tem.*|.*lag_prec.*', variables$names)
    lagged_sat <- grep(pattern = '.*lag_sc.*|.*lag_lwe.*', variables$names)
    
    for(j in 1:nrow(plotData)){
      plotData$predictorType[j] <- case_when((plotData$names[j] %in% c('precipitation',
                                                                       'temperature',
                                                                       'referencePotET')) ~'red',
                                            (plotData$names[j] %in% variables$names[lagged_meteo])~'#ec6969', 
                                            (plotData$names[j] %in% c('sm','lwe','sc'))~'blue',
                                            (plotData$names[j] %in% variables$names[lagged_sat])~'#584dff',
                                            .default = 'green')
    }
    
    labColor <- plotData$predictorType
    pTitle <- str_replace_all(str_sub(setup_name,start = 12),pattern = '_' , replacement = ' ')
    if (regions[1] != 'all'){
      pTitle <- paste(pTitle)
    }
    if (c == 'AU'){
      minorBreaks <- c(0.01,0.02,0.03)
      Breaks <- c(0.01,0.02,0.03)
      Limits <- c(0,0.035)
    }else if (c == 'US'){
      minorBreaks = c(0.05,0.1)
      Breaks = c(0.05,0.1)
      Limits = c(0,0.13)
    }else if (c == 'CA'){
      minorBreaks = c(0.05,0.1)
      Breaks = c(0.05,0.1)
      Limits = c(0,0.1)
    }else{
      minorBreaks = c(0.1,0.2,0.3)
      Breaks = c(0.1,0.2,0.3)
      Limits = c(0,0.35)
    }
    viPlot <- ggplot(plotData) +
        geom_col( aes(reorder(names, c(value[key=='importance_avg'])), sqrt(value)),
                 position = 'dodge', fill=labColor) +
        geom_errorbar(aes(reorder(names, c(value[key=='importance_avg'])),
                          ymin=sqrt(value)-sqrt(importance_sd), ymax=sqrt(value)+sqrt(importance_sd),
                          width=0.8, linewidth=0.1, colour="red"), show.legend = F) +
        # ylim(0,ymax)+
        scale_y_continuous(minor_breaks = minorBreaks, breaks = Breaks, limits = Limits)+
        coord_flip() +
        theme_light()+
        labs(x=NULL, y=NULL,
             title=pTitle)+
        theme(
          axis.text.y = element_text(size = 40),
          axis.text.x = element_text(size = 35),
          title = element_text(size = 35))
  
    viPlotList[[count]] <- viPlot
    count <- count + 1
      
  }
  # print(count)
  if (count == 2){
    trainingPlot <- viPlotList[[1]]
  }
  else if (count == 3){
    trainingPlot <- viPlotList[[1]] / viPlotList[[2]]
  }else if (count == 4){
    trainingPlot <- viPlotList[[1]] / viPlotList[[2]] / viPlotList[[3]]
  }else {'wrong settings'}
  
  setupPlotlist[[i]] <- trainingPlot
  if (regions[1] != 'all')
  ggsave(paste0(outputDir, paste0('varImportance_',setup_name,'.pdf')), trainingPlot, height=35, width=25,
  units='in', dpi=600)
}

if (regions[1] == 'all'){
  a <- ggarrange(setupPlotlist[[1]], setupPlotlist[[2]], setupPlotlist[[3]], ncol =3, nrow = 1)
  
  blank <- ggplot(plotData) + geom_blank() + theme_minimal()
  
  b <- ggarrange(blank, setupPlotlist[[4]], setupPlotlist[[5]],blank, ncol =4, nrow = 1, widths = c(1,2,2,1))
  
  c <- ggarrange(a, b , ncol = 1, nrow =2)
  
  ggsave(paste0(outputDir, paste0('varImportance_allsetups_all.pdf')), c, height=50, width=40,
         units='in', dpi=600, limitsize = F)
}else{
  blank <- ggplot(plotData) + geom_blank() + theme_minimal()
  
  a <- ggarrange(blank, setupPlotlist[[1]], setupPlotlist[[2]], blank , ncol =4, nrow = 1, widths =c(1,2,2,1))
  b <- ggarrange(blank, setupPlotlist[[3]], setupPlotlist[[4]], blank , ncol =4, nrow = 1, widths =c(1,2,2,1))
  
  
  c <- ggarrange(setupPlotlist[[5]], setupPlotlist[[6]], setupPlotlist[[7]], ncol =3, nrow = 1)
  
  d <- ggarrange(a, b, c , ncol = 1, nrow =3)
  
  ggsave(paste0(outputDir, paste0('varImportance_allsetups_',regions[1],'.pdf')), d, height=50, width=40,
         units='in', dpi=600, limitsize = F)
  
}
# ggsave(paste0(outputDir, paste0('varImportance_allsetups_all.png')), a, height=15, width=35,
#        units='in', dpi=600)



