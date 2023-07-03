####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####
library('ggh4x')


outputDir <- paste0('../../viz/')
dir.create(outputDir, showWarnings = F, recursive = T)



regions <- c('AU', 'CA', 'US')
# regions <- c('CA')
# regions <- c('US')
regions <- c('all')
samples <- 5

filePathRF <- paste0('../../data/')
setup <- list.files(filePathRF, pattern = 'predictors_')
if (regions[1] =='all'){
  setup <- c('predictors_pcr.csv','predictors_pcr_sat.csv', 'predictors_pcr_sat_add.csv',
                     'predictors_sat_meteo.csv', 'predictors_sat_meteo_static.csv')
  # ,
  #                    'predictors_pcrLim_meteo_static.csv', 'predictors_pcrLim_meteo.csv')
  plotLevels <- c('uncalibrated',
                  'pcr',
                  'pcr sat',
                  'pcr sat add',
                  'sat meteo',
                  'sat meteo static')
  plotLabels <- c('uncalibrated',
                  'pcr',
                  'pcrSat',
                  'pcrSatAdd',
                  'satMeteo',
                  'satMeteoStatic')
}else{
  setup <- setup[c(grep('4.csv', setup),
                                   grep('12.csv', setup),
                                   grep('sat.csv', setup),
                                   grep('sat_meteo.csv', setup),
                                   grep('pcr.csv', setup))]
  plotLevels <- c('uncalibrated',
                  'pcr',
                  'pcr sat',
                  'pcr sat lagged 4',
                  'pcr sat lagged 12',
                  'sat meteo',
                  'sat meteo lagged 4',
                  'sat meteo lagged 12')
  plotLabels <- c('uncalibrated',
                  'pcr',
                  'pcrSat',
                  'pcrSatLag4',
                  'pcrSatLag12',
                  'satMeteo',
                  'satMeteoLag4',
                  'satMeteoLag12')
}

colors <- c('red', '#4149FE', 'green', 'gray', 'yellow', 'cyan', '#f40bc1', '#ffa229')

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
  
  
  plotData$setup <- factor(plotData$setup, levels = plotLevels, labels = 
                             c(plotLabels))
  
  
  if (c == 'AU'){
    title = 'Australia'
    title_ax = element_text(hjust=0.5, size=20)
  }else if (c == 'CA'){
    title = 'Canada'
    title_ax = element_text(hjust=0.5, size=20)
  }else if (c == 'US'){
    title_ax = element_text(hjust=0.5, size=20)
    title = 'United States'
  }else{
    title_ax = element_blank()
  }
  #### plot boxplots subsample facets ####
  KGE_boxplot <- ggplot(plotData , mapping = aes(setup, value, fill=setup))+
    geom_boxplot(outlier.shape = NA) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_hline(yintercept = -0.41, linetype = "dashed") +
    facet_grid(vars(subsample), scales='free_y', switch='y')+
    labs(title = title)+
    scale_fill_manual(values=colors)+
    theme(plot.title = title_ax,
          axis.title=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y= element_text(size=12),
          legend.position = 'bottom',
          legend.title=element_blank(),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=14),
          strip.text = element_text(size=12))+
     coord_cartesian(ylim=c(-3,1))
  KGE_boxplot
  
  ggsave(paste0(outputDir,'KGE_boxplots_',c,'.pdf'), KGE_boxplot, height=13, width=10, units='in', dpi=600)
}



