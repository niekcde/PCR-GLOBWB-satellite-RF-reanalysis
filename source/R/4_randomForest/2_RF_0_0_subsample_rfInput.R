####-------------------------------####
source('fun_0_loadLibrary.R')
source('project_settings.R')
####-------------------------------####

create_subsample_rf_input <- function(country, runs, samples){
  # open en select correct stations 
  run_variables <- c()
  for (i in runs){
    variables <- read.csv(paste0('../../data/', i,'.csv'))$names
    run_variables <- c(run_variables, variables)
  }
  selected_variables <- unique(run_variables)
  print(country)
  
  stationInfo <- read.csv(paste0('../../data/stationLatLon_selected_', country,'.csv'))
  for (i in 1:samples){
    print(paste0('subsample: ', print(i)))
    subsample <- i
    
    outputDir <- paste0('../../RF/0_rf_input/subsample_',subsample,'_',country,'/')
    print(outputDir)
    dir.create(outputDir, showWarnings = F, recursive = T)
    
    # Determine the number of stations for training. Depends on the number of stations in the dataset
    number_train_samples <- floor(0.7*nrow(stationInfo))
    
    #---- subsample such that train_stations has between 2/3 and 70% of available data ----#
    source('2_randomForest/fun_2_0_subsample_train_test.R')
    registerDoParallel(12)
    print('sampling...')
    repeat{
      ## subset train station, select and read file tables, collect, read nrow
      #number of train stations depends on whole set dimension (~70%)
      
      train_stations <- stationInfo[sample(nrow(stationInfo),number_train_samples),] 
      train_table <- subsample_table(train_stations, selected_variables) %>% 
    					mutate(datetime=as.Date(datetime))
      nrow_train <- nrow(train_table)
      print('finished: train dataset')
      
      ## same for test stations
      test_stations <- setdiff(stationInfo, train_stations)
      test_table <- subsample_table(test_stations, selected_variables)
      nrow_test <- nrow(test_table)
      
      print('finished: test dataset')
      
      ratio_subsamples <- nrow_train/(nrow_train+nrow_test)
      print(ratio_subsamples)
      if(ratio_subsamples > 0.66 & ratio_subsamples < 0.7){
        print(paste0('subsample successful! writing....'))
        break
        } else{
        print('subsample failed :/ train dataset too small/big... resampling...')
        }
    
    } #end repeat loop
    
    for (run in runs){
      selected_variables <- read.csv(paste0('../../data/', run,'.csv'))$names
      
      train_table <- subsample_table(train_stations, selected_variables) %>% 
        mutate(datetime=as.Date(datetime))
      write.csv(train_table, paste0(outputDir,'train_table_',run,'.csv'), row.names = F)
    }
    # write tables: train_stations, test_stations, train_table
    write.csv(train_stations, paste0(outputDir,'train_stations.csv'), row.names = F)
    write.csv(test_stations, paste0(outputDir,'test_stations.csv'), row.names = F)
    
  } #end subsample loop
} # end of function

filePathPreds <- paste0('../../data/predictors/pcr_allpredictors_all/')
fileListPreds <- list.files(filePathPreds, pattern='.csv')
filenames <- paste0(filePathPreds, fileListPreds)

countries <- c('US', 'AU', 'CA', 'all')
countries <- c('all')

samples <- 5
for (country in countries){
  if (country !='all'){
    runs <- c('predictors_pcr_sat_lagged_4', 'predictors_pcr_sat_lagged_12',
              'predictors_sat_meteo_lagged_4', 'predictors_sat_meteo_lagged_12',
              'predictors_pcr_sat', 'predictors_pcr', 'predictors_sat_meteo',
              'predictors_sat_meteo_static', 'predictors_pcrLim_meteo_static',
              'predictors_pcrLim_meteo')
  }else{runs <- c('predictors_pcr','predictors_pcr_sat', 'predictors_pcr_sat_add',
                  'predictors_sat_meteo', 'predictors_sat_meteo_static')}
    create_subsample_rf_input(country, runs, samples)
}


# if new configurations are added after the train test split has been determined ####
runs <-c('predictors_sat_meteo_static')
countries <- c('all')
source('2_randomForest/fun_2_0_subsample_train_test.R')
# train test split already made
for (country in countries){
  for (subsample in 1:samples){
    
    outputDir <- paste0('../../RF/0_rf_input/subsample_',subsample,'_',country,'/')
    print(outputDir)
    dir.create(outputDir, showWarnings = F, recursive = T)
    
    for (run in runs){
      selected_variables <- read.csv(paste0('../../data/', run,'.csv'))$names
      train_stations <- read.csv(paste0('../../RF/0_rf_input/subsample_',
                                        subsample,'_',country,'/train_stations.csv'))
      train_table <- subsample_table(train_stations, selected_variables) %>%
        mutate(datetime=as.Date(datetime))
      write.csv(train_table, paste0(outputDir,'train_MRF_table_',run,'.csv'), row.names = F)
    }
  }
}






