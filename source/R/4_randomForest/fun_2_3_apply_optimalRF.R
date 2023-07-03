# function to apply a trained RF to unseen data
# it writes complete tables for allpredictors and stores KGE for all setups

# key = qstatevars, allpredictors
apply_optimalRF <- function(i, key){
    

    station_no <- testStationInfo$grdc_no[i]

    test_data <- read.csv(paste0('../../data/predictors/pcr_allpredictors_all/pcr_allpredictors_',
								 station_no, '.csv'))
    
    variables <- read.csv(paste0('../../data/', key,'.csv'))
    variables_noobs <- variables %>% filter(!names == 'obs')
    test_data <- test_data %>% select(datetime,pcr, all_of(variables$names)) %>% drop_na(any_of(variables_noobs$names))
    # test_data <- test_data %>% drop_na(any_of('sm'))
    # print('step 2')
    
    if (nrow(test_data) != 0){
      rf.result <- test_data %>% 
        # predict discharge with trained RF
          mutate(pcr_corrected = predict(optimal_ranger, test_data) %>% predictions()) %>%
        # if pcr_corrected < 0 -> pcr_corrected=0
          mutate(pcr_corrected = replace(pcr_corrected, pcr_corrected < 0,0)) %>%
        # calculate residuals
          mutate(res=obs-pcr) %>%
          mutate(res_corrected=obs-pcr_corrected) %>%
        # move new discharge variables before state variables
        relocate(pcr_corrected, .before=precipitation) %>%
        relocate(res, .before=precipitation) %>%
        relocate(res_corrected, .before=precipitation) %>%
        #keep only datetime, obs, pcr, pcr_corrected, res, res_corrected
          select(.,datetime,obs,pcr, pcr_corrected, res,res_corrected)
    }else{rf.result <- test_data}
    # print('step 3')
    # predictor tables to disk
    
      outputDirTables <- paste0(outputDirValidation, 'tables_',key, '/')
      dir.create(outputDirTables, showWarnings = F, recursive = T)
      write.csv(rf.result, paste0(outputDirTables, 'rf_result_',
                                  station_no, '.csv'), row.names = F)
       
    # print('step 4')
    # rf.result <- tibble(datetime = c(2003-02-01,2003-03-01) ,obs  = c(0,1)        ,pcr =c(3.425126e-07,3.825126e-07)
    #                     ,pcr_corrected = c(0.000547299,0.000647299)          ,res = c(-3.425126e-07,-3.525126e-07)
    #                     ,res_corrected=c(-0.000547299,-0.000577299))
      
      
    na_obs <- test_data %>% na.omit(.)
    # print(rf.result)
    # print(na_obs)
    # print(rf.result %>% na.omit(.))
    if (nrow(na_obs) <= 1){
      # print('step 5.1')
      rf.eval <- rf.result %>%
        summarise(grdc_no=station_no,
                  KGE=NA,
                  KGE_corrected=NA,
                  # KGE components (r,alpha,beta), uncalibrated and corrected pcrglob
                  KGE_r=NA,
                  KGE_r_corrected=NA,
                  KGE_alpha=NA,
                  KGE_alpha_corrected=NA,
                  KGE_beta=NA,
                  KGE_beta_corrected=NA,
                  # other metrics
                  NSE = NA,
                  NSE_corrected = NA,
                  RMSE=NA,
                  RMSE_corrected=NA,
                  MAE=NA,
                  MAE_corrected=NA,
                  nRMSE=NA,
                  nRMSE_corrected=NA,
                  nMAE=NA,
                  nMAE_corrected=NA)
    }else{
    # print('step 5.2')
    #calculate KGE uncalibrated and corrected
      rf.eval <- rf.result %>%
        summarise(grdc_no=station_no,
                  KGE=KGE(sim = pcr, obs = obs,
                          s = c(1,1,1), na.rm = T, method = "2009"),
                KGE_corrected=KGE(sim = pcr_corrected, obs = obs,
                s = c(1,1,1), na.rm = T, method = "2009"),
                # KGE components (r,alpha,beta), uncalibrated and corrected pcrglob
                  KGE_r=cor(obs,pcr,method='pearson',use='complete.obs'),
                  KGE_r_corrected=cor(obs,pcr_corrected,method='pearson',use='complete.obs'),
                  KGE_alpha=sd(pcr, na.rm=T)/sd(obs, na.rm=T),
                  KGE_alpha_corrected=sd(pcr_corrected, na.rm=T)/sd(obs, na.rm=T),
                  KGE_beta=mean(pcr, na.rm=T)/mean(obs, na.rm=T),
                  KGE_beta_corrected=mean(pcr_corrected, na.rm=T)/mean(obs, na.rm=T),
                # other metrics
                  NSE = NSE(sim = pcr, obs = obs, na.rm = T),
                  NSE_corrected = NSE(sim = pcr_corrected, obs = obs, na.rm = T),
                  RMSE=(((res)^2) %>% mean(na.rm=T) %>% sqrt),
                  RMSE_corrected=(((res_corrected)^2) %>% mean(na.rm=T) %>% sqrt),
                  MAE=res %>% abs %>% mean(na.rm=T),
                  MAE_corrected=res_corrected %>% abs %>% mean(na.rm=T),
                  nRMSE=(((res)^2) %>% mean(na.rm=T) %>% sqrt)/mean(obs),
                  nRMSE_corrected=(((res_corrected)^2) %>% mean(na.rm=T) %>% sqrt)/mean(obs),
                  nMAE=(res %>% abs %>% mean(na.rm=T))/mean(obs),
                  nMAE_corrected=(res_corrected %>% abs %>% mean(na.rm=T))/mean(obs)
                )
    }
    return(rf.eval)
}
