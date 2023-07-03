####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####

outputDir <- '../../viz/varImportance/'
dir.create(outputDir, showWarnings = F, recursive = T)

# static #####
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

#choose runs ####
regions <- c('AU')
# regions <- c('CA')
regions <- c('US')

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
                                                                       'referencePotET')) ~'Meteorological input',
                                            (plotData$names[j] %in% variables$names[lagged_meteo])~'Lag Meteorological input', 
                                            (plotData$names[j] %in% c('sm','lwe','sc'))~'Satellite products',
                                            (plotData$names[j] %in% variables$names[lagged_sat])~'Lag Satellite products',
                                            (plotData$names[j] %in% static)~'Static variables',
                                            .default = 'State variables')
    }
    
    labColor <- plotData$predictorType
    pTitle <- str_split(str_sub(setup_name,start = 12), pattern = '_' )[[1]]
    if (length(pTitle) > 1){
      pTitle <- paste0(pTitle[1], paste0(str_to_title(pTitle[2:length(pTitle)]), collapse = ''))
    }
    

    if (c == 'all'){
      plotData$predictorType <- factor(plotData$predictorType, levels = c('State variables', 'Satellite products', 'Meteorological input',
                                                                          'Static variables'))
      colors <- c("green", "blue", "red", 'darkgreen')
      }else{
        plotData$predictorType <- factor(plotData$predictorType, levels = c('State variables', 'Static variables',
                                                                            'Satellite products','Lag Satellite products',
                                                                            'Meteorological input','Lag Meteorological input'))
        colors <- c("green",'darkgreen', "blue", '#37e1e1', "red", '#e66464')
      }
    
    plotData <- plotData %>% mutate(names = ifelse(names == 'sc', 'scf', names))
    
    viPlot <- ggplot(plotData) +
        geom_col(aes(reorder(names, c(value[key=='importance_avg'])), sqrt(value), fill = predictorType),
                 position = 'dodge')+#, fill=labColor) +
        geom_errorbar(aes(reorder(names, c(value[key=='importance_avg'])),
                          ymin=sqrt(value)-sqrt(importance_sd), ymax=sqrt(value)+sqrt(importance_sd),
                          width=0.8, linewidth=0.1, colour="red"), show.legend = F) +
        # ylim(0,ymax)+
        # scale_y_continuous(minor_breaks = minorBreaks, breaks = Breaks, limits = Limits)+
        coord_flip() +
        theme_light()+
        labs(x=NULL, y=NULL,
             title=pTitle)+
        scale_fill_manual(values=colors, drop = F)+
        theme(
          axis.text.y = element_text(size = 40),
          axis.text.x = element_text(size = 40),
          title = element_text(size = 50),
          legend.title=element_blank(),
          legend.key.size = unit(3, 'cm'),
          legend.text = element_text(size=50),
          legend.spacing.x = unit(1, 'cm'))
    ggsave(paste0('../../viz/varImportance/', c,'_',setup[i], '.pdf'),viPlot,height=20, width=20, units='in', dpi=600) 
    
  }
}


