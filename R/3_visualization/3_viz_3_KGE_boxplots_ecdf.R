####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####
library('ggh4x')


outputDir <- paste0('../../viz/')
dir.create(outputDir, showWarnings = F, recursive = T)

filePathRF <- paste0('../../data/')
setup <- list.files(filePathRF, pattern = 'predictors_')
#choose runs
setup <- setup[c(#grep('12', setup), 
  grep('4.csv', setup), 
  grep('pcr.csv', setup),
  grep('pcr_sat.csv', setup), 
  grep('sat_meteo.csv', setup),
  grep('static.csv', setup))]


# setup <- c('predictors_pcr.csv','predictors_pcr_sat.csv', 'predictors_pcr_sat_add.csv',
#           'predictors_sat_meteo.csv', 'predictors_sat_meteo_static.csv')


regions <- c('AU', 'CA', 'US')
# regions <- c('all')
samples <- 5



colors <- c('red', 'blue', 'green', 'black', 'yellow', 'cyan', '#f40bc1', '#ffa229')

read_metrics <- function(setup, subsample){
  print(setup)
  setup <- str_sub(setup, end = -5)
  setup_factor <- gsub('_', ' ', str_sub(setup, start = 12))
  print(setup_factor)
  print(setup)
  rf.eval.meteoCatchAttr <- read.csv(paste0('../../RF/3_validate/subsample_', subsample,'_',c,
                                            '/KGE_',setup,'.csv')) %>% 
    select(.,grdc_no, KGE_corrected, KGE_r_corrected, KGE_alpha_corrected, KGE_beta_corrected) %>% 
    rename(., KGE=KGE_corrected, KGE_r=KGE_r_corrected, KGE_alpha=KGE_alpha_corrected, KGE_beta=KGE_beta_corrected) %>% 
    mutate(.,setup=factor(setup_factor)) %>% 
    mutate(.,subsample=factor(subsample)) 
}

#### data preparation ####
for (c in regions){
  subsample_KGE_list <- list ()
  setup_name <- str_sub(setup[i], end = -5)
  for(subsample in 1:samples){
    
    rf.eval.uncalibrated <- read.csv(paste0('../../RF/3_validate/subsample_', subsample,'_',c,
                                            '/KGE_predictors_pcr.csv')) %>%
      select(.,grdc_no, KGE, KGE_r, KGE_alpha, KGE_beta) %>%
      mutate(.,setup=factor('uncalibrated')) %>%
      mutate(.,subsample=factor(subsample))
    
    subsample_KGE <- rf.eval.uncalibrated
    for (s in setup){
      rf.eval        <- read_metrics(s, subsample)
      subsample_KGE <- rbind(subsample_KGE, rf.eval)
    }
    
    subsample_KGE_list[[subsample]] <- subsample_KGE
  }
  allData <- do.call(rbind, subsample_KGE_list)
  allDataCum <- allData %>% mutate(subsample='C')
  allData <- rbind(allData,allDataCum)
  
  
  plotData <- allData %>% pivot_longer(KGE:KGE_beta, names_to = "KGE_component", 
                                       values_to = "value") %>% 
    mutate(KGE_component = fct_relevel(KGE_component, 'KGE','KGE_r','KGE_alpha','KGE_beta'))
  # plotData <- plotData %>% filter(subsample == 'Cumulative')
  plotData <- plotData %>%  filter(KGE_component == 'KGE')
  

  
  plotLevels <- c('uncalibrated',
                  'pcr',
                  'pcr sat',
                  # 'pcr sat add',
                   'pcr sat lagged 4',
                  # 'pcr sat lagged 6',
                  # 'pcr sat lagged 8',
                  # 'pcr sat lagged 12',
                  'sat meteo',
                  'sat meteo static',
                  'sat meteo lagged 4',
                  # 'sat meteo lagged 6',
                  # 'sat meteo lagged 8',
                   'sat meteo lagged 12'
                  # 'pcr sat lagged 4 sm',
                  # 'pcr sat lagged 12 sm',
                  # 'sat meteo lagged 4 sm',
                  # 'sat meteo lagged 12 sm'
                  )
  plotData$setup <- factor(plotData$setup, levels = plotLevels)

  
  #### plot boxplots ####
  KGE_boxplot <- ggplot(plotData , mapping = aes(setup, value, fill=setup))+
    geom_boxplot(outlier.shape = NA) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    facet_grid(vars(KGE_component), vars(subsample), scales='free_y', switch='y')+
    labs(title = c)+
    scale_fill_manual(values=colors)+
    theme(plot.title = element_text(hjust=0.5, size=16),
          axis.title=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y= element_text(size=12),
          legend.position = 'bottom',
          legend.title=element_blank(),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=14),
          strip.text = element_text(size=12))+
    facetted_pos_scales(y = list(
      KGE_component == "KGE" ~ scale_y_continuous(position='right', limits = c(-2, 1)),
      KGE_component == "KGE_r" ~ scale_y_continuous(position='right', limits = c(0, 1)),
      KGE_component == "KGE_alpha" ~ scale_y_continuous(position='right', limits = c(-0.5,5)),
      KGE_component == "KGE_beta" ~ scale_y_continuous(position='right', limits = c(-0.5,5.5))))
  KGE_boxplot
  
  #### plot boxplots subsample facets ####
  KGE_boxplot <- ggplot(plotData , mapping = aes(setup, value, fill=setup))+
    geom_boxplot(outlier.shape = NA) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_hline(yintercept = -0.41, linetype = "dashed") +
    facet_grid(vars(subsample), scales='free_y', switch='y')+
    labs(title = c)+
    scale_fill_manual(values=colors)+
    theme(plot.title = element_blank(),
          axis.title=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y= element_text(size=12),
          legend.position = 'bottom',
          legend.title=element_blank(),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=14),
          strip.text = element_text(size=12))+
    scale_y_continuous(position = 'right', limits = c(-1,1), breaks = c(1, 0.0,-1))
    # facetted_pos_scales(y = list(
      # KGE_component == "KGE" ~ scale_y_continuous(position='right', limits = c(-2, 1)),
      # KGE_component == "KGE_r" ~ scale_y_continuous(position='right', limits = c(0, 1)),
      # KGE_component == "KGE_alpha" ~ scale_y_continuous(position='right', limits = c(-0.5,5)),
      # KGE_component == "KGE_beta" ~ scale_y_continuous(position='right', limits = c(-0.5,5.5))))
  KGE_boxplot
  
  ggsave(paste0(outputDir,'KGE_boxplots_',c,'.pdf'), KGE_boxplot, height=10, width=10, units='in', dpi=600)
}



#### some stats (paper section 3.3.) ####
uncalibratedCum <- allDataCum %>% filter(setup=='uncalibrated')
pcr_predictorsCum <- allDataCum %>% filter(setup=='pcr_predictors')
pcr_sat_predictorsCum <- allDataCum %>% filter(setup=='pcr_sat_predictors')
sat_predictorsCum <- allDataCum %>% filter(setup=='sat_predictors')
sat_meteo_predictorsCum <- allDataCum %>% filter(setup=='sat_meteo_predictors')

summary(uncalibratedCum$KGE)
summary(pcr_predictorsCum$KGE)
summary(pcr_sat_predictorsCum$KGE)
summary(sat_predictorsCum$KGE)
summary(sat_meteo_predictorsCum$KGE)

summary(uncalibratedCum$KGE_r)
summary(pcr_predictorsCum$KGE_r)
summary(pcr_sat_predictorsCum$KGE_r)
summary(sat_predictorsCum$KGE_r)
summary(sat_meteo_predictorsCum$KGE_r)

summary(uncalibratedCum$KGE_alpha)
summary(pcr_predictorsCum$KGE_alpha)
summary(pcr_sat_predictorsCum$KGE_alpha)
summary(sat_predictorsCum$KGE_alpha)
summary(sat_meteo_predictorsCum$KGE_alpha)

summary(uncalibratedCum$KGE_beta)
summary(pcr_predictorsCum$KGE_beta)
summary(pcr_sat_predictorsCum$KGE_beta)
summary(sat_predictorsCum$KGE_beta)
summary(sat_meteo_predictorsCum$KGE_beta)
