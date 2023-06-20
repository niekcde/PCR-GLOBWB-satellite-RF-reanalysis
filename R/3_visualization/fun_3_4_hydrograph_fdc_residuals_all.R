# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")  #with grey
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #with black

plot_hydrograph <- function(station_no, convRatio, setup){
  
  dir <- paste0('../../RF/3_validate/subsample_', subsample,'_',country, '/tables_predictors_')
  
  
  for (s in 1:length(setup)){
    if (s == 1){
      all_q <- read.csv(paste0(dir, setup[s], '/rf_result_', station_no,'.csv'), header = T) %>%
              mutate(datetime=as.Date(datetime)) %>% select(datetime, obs,pcr_corrected)
      names(all_q) <- c('datetime','obs',setup[s])
      
    }else{
      file2 <- read.csv(paste0(dir, setup[s], '/rf_result_', station_no,'.csv'), header = T) %>%
        mutate(datetime=as.Date(datetime)) %>% select(datetime, pcr_corrected)
      names(file2) <- c('datetime',setup[s])
      all_q <- merge(all_q, file2, by = 'datetime')
      }
  }
  
  
  plotData <- all_q %>% 
    pivot_longer(cols = c('obs', all_of(setup)),
                 names_to = 'condition', values_to = 'value')

  ggplot(plotData,
         aes(x=datetime, y=value, col=condition, group = condition)) +
    geom_line(linewidth=0.7, alpha = 0.6) +
    geom_point(aes(shape=condition), size=1.5, alpha = 0.6, show.legend = F) +
    xlab('date\n') + 
    ylab('flow depth (m/d)\n') +
    scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression('discharge (' *m^{3}/s* ')'))) +
    scale_color_manual(labels = c("Observed discharge   ",str_to_title(gsub('_', ' ', setup ))),
                       values=c("#56B4E9","#000000","#E69F00", '#645197', '#ffdf20'))+
    scale_linetype_manual(values=1:5, 
                          labels = c("Observed discharge   " ,str_to_title(gsub('_', ' ', setup )))) +
    theme_minimal() +
    theme(legend.title=element_blank(),
          legend.text = element_text(size=16),
          axis.title.x = element_text(face="bold", size=16),
          axis.title.y = element_text(size=16),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12))+
    guides(colour = guide_legend(override.aes = list(size=3)))
 }


plot_ecdf <- function(station_no, convRatio, setup){
  dir <- paste0('../../RF/3_validate/subsample_', subsample,'_',country, '/tables_predictors_')
  

  # station_no <- 6140400
  for (s in 1:length(setup)){
    if (s == 1){
      all_q <- read.csv(paste0(dir, setup[s], '/rf_result_', station_no,'.csv'), header = T) %>%
        mutate(datetime=as.Date(datetime)) %>% select(datetime, obs,pcr_corrected)
      names(all_q) <- c('datetime','obs',setup[s])
      
    }else{
      file2 <- read.csv(paste0(dir, setup[s], '/rf_result_', station_no,'.csv'), header = T) %>%
        mutate(datetime=as.Date(datetime)) %>% select(datetime, pcr_corrected)
      names(file2) <- c('datetime',setup[s])
      all_q <- merge(all_q, file2, by = 'datetime')
    }
  }
  
  
  plotData <- all_q %>% 
    pivot_longer(cols = c('obs', all_of(setup)),
                 names_to = 'condition', values_to = 'value')
  
  ggplot(plotData, aes(value, col=condition)) +
    stat_ecdf(size=1, alpha=0.6, show.legend = F)+
    # scale_x_continuous(sec.axis = sec_axis(~.*convRatio, name=expression('discharge (' *m^{3}/s* ')'))) +
    coord_flip() +
    xlab('flow depth (m/d)\n') + 
    ylab('p\n') +
    scale_color_manual(values=c("#56B4E9","#000000","#E69F00", '#645197', '#ffdf20')) +
    theme_minimal() +
    theme(axis.title.x = element_text(face="bold", size=16),
          axis.title.y = element_text(size=16),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12))

}


plot_residuals <- function(station_no, convRatio, setup){
  dir <- paste0('../../RF/3_validate/subsample_', subsample,'_',country, '/tables_predictors_')

  # station_no <- 6140400
  for (s in 1:length(setup)){
    if (s == 1){
      all_q <- read.csv(paste0(dir, setup[s], '/rf_result_', station_no,'.csv'), header = T) %>%
        mutate(datetime=as.Date(datetime)) %>% select(datetime, obs, res_corrected)
      names(all_q) <- c('datetime','obs',setup[s])
      
    }else{
      file2 <- read.csv(paste0(dir, setup[s], '/rf_result_', station_no,'.csv'), header = T) %>%
        mutate(datetime=as.Date(datetime)) %>% select(datetime, res_corrected)
      names(file2) <- c('datetime',setup[s])
      all_q <- merge(all_q, file2, by = 'datetime')
    }
  }
  
  
  plotData <- all_q %>% 
    pivot_longer(cols = c( all_of(setup)),
                 names_to = 'condition', values_to = 'value')
  
  
  # all_q <- read.csv(paste0(dir, 'rf_result_', station_no,'.csv'), header = T) %>%
  #   mutate(datetime=as.Date(datetime)) %>% select(., datetime:res_corrected) %>%
  #   na.omit()
  # 
  # plotData <- all_q %>% select(-c('pcr','pcr_corrected')) %>% 
  #   gather(.,condition, value, res:res_corrected)
  
  ggplot(plotData, aes(x=obs, y=value, col=condition)) +
    geom_point(size=2,alpha=0.6, show.legend = F)+
    # scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression('residual (\n' *m^{3}/s* ') \n'))) +
    # scale_y_continuous(position = "right")+
    xlab('observed flow depth (m/d)\n') +
    ylab('residual (m/d)\n') +
    scale_color_manual(values=c("#000000","#E69F00", '#645197', '#ffdf20')) +
    theme_minimal() +
    theme(axis.title.x = element_text(face="bold", size=16),
          axis.title.y = element_text(size=16),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12))
  
}
