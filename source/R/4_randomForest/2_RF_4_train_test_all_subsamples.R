####-------------------------------####
source('fun_0_loadLibrary.R')
source('project_settings.R')
####-------------------------------####
source('2_randomForest/fun_2_2_trainRF.R')
source('2_randomForest/fun_2_3_apply_optimalRF.R')

#-------train RF with tuned parameters on 70% of available observations----------
trees <- 1000 # almost always 700 trees so stick to 700


countries <- c('AU', 'CA', 'US')
countries <- c('all')

for (country in countries){
  tuned_mtry <- read.csv(paste0('../../RF/1_tune/tuned_mtry_',country,'.csv'), header=T)  %>% 
    select(-X)
  
  filePathRF <- paste0('../../data/')
  RF_input_list <- list.files(filePathRF, pattern = 'predictors_')
  if (country =='all'){
    RF_input_list <- c('predictors_pcr.csv','predictors_pcr_sat.csv', 'predictors_pcr_sat_add.csv',
            'predictors_sat_meteo.csv', 'predictors_sat_meteo_static.csv')
  }else{
    RF_input_list <- RF_input_list[c(grep('4.csv', RF_input_list),
                 grep('12.csv', RF_input_list),
                 grep('sat.csv', RF_input_list),
                 grep('meteo.csv', RF_input_list),
                 grep('pcr.csv', RF_input_list),
                 grep('static.csv', RF_input_list))]
  }
  
  for (RF in RF_input_list){
    output <- str_sub(RF, end = -5)
    print(output)
    
    for(subsample in 1:samples){
      
        print(paste0('subsample: ', subsample))
        #select subsample predictors
        train_data <- vroom(paste0('../../RF/0_rf_input/', 'subsample_',subsample,'_',country,
                                   '/train_table_',output,'.csv'), show_col_type=F)
        testStationInfo <- read.csv(paste0('../../RF/0_rf_input/subsample_',subsample,'_',
                                           country,'/test_stations.csv'))
        
        outputDir <- paste0('../../RF/2_train/subsample_',subsample,'_',country,'/')
        dir.create(outputDir, showWarnings = F, recursive = T)
        outputDirValidation <- paste0('../../RF/3_validate/subsample_',subsample,'_',country,'/')
        dir.create(outputDirValidation, showWarnings = F, recursive = T)
        
        
        rf_input <- train_data %>% select(-datetime)
        
        mtry <- tuned_mtry[subsample, output]
        
        optimal_ranger <- trainRF(input_table=rf_input, num.trees=trees, mtry=mtry)
    
        vi_df <- data.frame(names=names(optimal_ranger$variable.importance)) %>%
          mutate(importance=optimal_ranger$variable.importance) %>% arrange(importance)                    
        write.csv(vi_df, paste0(outputDir,paste0('varImportance_',output,'.csv')), row.names=F)
    
        #run validation script
        key=output
        print(paste0(key,' : calculation initiated...'))
        
        KGE_lists <- list()
        
        for (i in 1:nrow(testStationInfo)){
          KGE_lists[[i]] <- apply_optimalRF(i, key)
          
        }
        print('test')
        rf.eval <- do.call(rbind,KGE_lists)
        
        # KGE_list <- mclapply(1:nrow(testStationInfo), key=key, apply_optimalRF, mc.cores=cores)
        # rf.eval <- do.call(rbind,KGE_list)
        write.csv(rf.eval, paste0(outputDirValidation, 'KGE_' , key, '.csv'), row.names = F)
        
        # print(paste0(key,' :  finished validation...'))
      
    }
  }
}