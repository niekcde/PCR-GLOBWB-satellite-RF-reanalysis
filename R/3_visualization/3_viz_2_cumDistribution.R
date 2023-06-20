####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####



outputDir <- '../../viz/'
dir.create(outputDir, showWarnings = F, recursive = T)


regions <- c('AU', 'CA', 'US')
# regions <- c('all')
samples <- 5
kge_plot_list <-list()
plot_text_size <- 30
if (regions[1] == 'all' | regions[1] == 'US_nomiss'){
  setup <- c('predictors_pcr.csv','predictors_pcr_sat.csv', 'predictors_pcr_sat_add.csv',
             'predictors_sat_meteo.csv', 'predictors_sat_meteo_static.csv')
  plotLevels <- c('uncalibrated',
                  'pcr',
                  'pcr sat',
                  'pcr sat add',
                  'sat meteo',
                  'sat meteo static')
  xtitle <- element_blank()
  xticks <- element_blank()
  ytitle <- element_text(size = plot_text_size)
  yticks <- element_text(size = plot_text_size)
  title <- c('KGE')
}else{
  filePathRF <- paste0('../../data/')
  setup <- list.files(filePathRF, pattern = 'predictors_')
#choose runs
  setup <- setup[c(#grep('12.csv', setup), 
    grep('4.csv$', setup), 
    grep('pcr.csv', setup),
     grep('pcr_sat.csv', setup), 
    grep('sat_meteo.csv', setup),
    grep('sat_meteo_static.csv', setup)
    )]
  
  plotLevels <- c('uncalibrated',
                  'pcr',
                   'pcr sat',
                   'pcr sat lagged 4',
                  #'pcr sat lagged 12',
                  'sat meteo',
                  'sat meteo static',
                  'sat meteo lagged 4'
                  #'sat meteo lagged 12'
                  )
  xtitle <- element_text(size = plot_text_size)
  xticks <- element_text(size = plot_text_size)
  ytitle <- element_text(size = plot_text_size)
  yticks <- element_text(size = plot_text_size)
  
  title <- regions
}



colors <- c('red', 'blue', 'green', 'black', 'yellow', 'cyan', '#f40bc1', 'gray', '#8639db', '#ff5800', '#d8f8f5', '#efe0ef')

#### data preparation ####
region_count <- 1
for (c in regions){
  subsample_KGE_list <- list ()

  for(subsample in 1:samples){
    
    rf.eval.uncalibrated <- read.csv(paste0('../../RF/3_validate/subsample_', subsample,'_',c,
                                            '/KGE_predictors_pcr.csv')) %>%
      select(.,grdc_no, KGE, KGE_r, KGE_alpha, KGE_beta) %>%
      mutate(.,setup=factor('uncalibrated')) %>%
      mutate(.,subsample=factor(subsample))
    # 
    read_metrics <- function(setup, subsample){
      setup <- str_sub(setup, end = -5)
      setup_factor <- gsub('_', ' ', str_sub(setup, start = 12))
      print(setup_factor)
      print(setup)
      rf.eval <- read.csv(paste0('../../RF/3_validate/subsample_', subsample,'_',c,
                                                '/KGE_',setup,'.csv')) %>% 
        select(.,grdc_no, KGE_corrected, KGE_r_corrected, KGE_alpha_corrected, KGE_beta_corrected) %>% 
        rename(., KGE=KGE_corrected, KGE_r=KGE_r_corrected, KGE_alpha=KGE_alpha_corrected, KGE_beta=KGE_beta_corrected) %>% 
        mutate(.,setup=factor(setup_factor)) %>% 
        mutate(.,subsample=factor(subsample)) 
      return (rf.eval)
    }
    subsample_KGE <- rf.eval.uncalibrated
    for (s in setup){
      rf.eval        <- read_metrics(s, subsample)
      subsample_KGE <- rbind(subsample_KGE, rf.eval)
    }
    
    subsample_KGE_list[[subsample]] <- subsample_KGE
  }
  allData <- do.call(rbind, subsample_KGE_list)
  # allDataCum <- allData %>% mutate(subsample='Cumulative')
  # allData <- rbind(allData,allDataCum)
  
  # print(c)
  # plotData$subsample
  
  plotData <- allData %>% group_by(grdc_no, setup) %>% summarise(mean_KGE = mean(KGE),
                                                                 mean_KGE_r = mean(KGE_r),
                                                                 mean_KGE_a = mean(KGE_alpha),
                                                                 mean_KGE_b = mean(KGE_beta)
                                                                 ) 
  print(unique(plotData$setup))
  
  plotData$setup <- factor(plotData$setup, levels = plotLevels)
  # KGE plot ####
  if (region_count == 1){
    ytitle <- element_text(size = plot_text_size)
    yticks <- element_text(size = plot_text_size)
  }else{
    ytitle <- element_blank()
    yticks <- element_blank()
  }
  plot <- ggplot(plotData, aes(mean_KGE, color = setup)) +
    stat_ecdf(geom = 'step') +
    xlim(-1, 1)+
    labs(title = title[region_count], y = 'p', x='Mean')+
    scale_color_manual(values=colors)+
    theme(plot.title = element_text(hjust=0.5, size=plot_text_size),
          axis.title.y = ytitle,
          axis.text.y  = yticks,
          axis.title.x = xtitle,
          axis.text.x  = xticks,
          
          legend.position = 'bottom',
          legend.title=element_blank(),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=plot_text_size),
          strip.text = element_text(size=plot_text_size))
   kge_plot_list[[region_count]] <- plot
   region_count <- region_count +1
  # KGE r plot ######
  plotr <- ggplot(plotData, aes(mean_KGE_r, color = setup)) +
    stat_ecdf(geom = 'step') +
    xlim(-1, 1)+
    labs(title = 'KGE: r')+
    scale_color_manual(values=colors)+
    theme(plot.title = element_text(hjust=0.5, size=plot_text_size),
          axis.title.y =element_blank(),
          axis.text.y= element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          
          legend.position = 'bottom',
          legend.title=element_blank(),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=plot_text_size),
          strip.text = element_text(size=plot_text_size))
  # KGE alpha plot ####
  plota <- ggplot(plotData, aes(mean_KGE_a, color = setup)) +
    stat_ecdf(geom = 'step') +
    xlim(-1, 1)+
    labs(title = expression('KGE: '~ alpha), x = 'Mean', y = 'p')+
    scale_color_manual(values=colors)+
    theme(plot.title = element_text(hjust=0.5, size=plot_text_size),
          axis.title =element_text(size=plot_text_size),
          axis.text.x=element_text(size=plot_text_size),
          axis.text.y= element_text(size=plot_text_size),
          legend.position = 'bottom',
          legend.title=element_blank(),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=plot_text_size),
          strip.text = element_text(size=plot_text_size))
  # KGE beta plot ####
  plotb <- ggplot(plotData, aes(mean_KGE_b, color = setup)) +
    stat_ecdf(geom = 'step') +
    xlim(-1, 1)+
    labs(title = expression('KGE: '~beta), x = 'Mean')+
    scale_color_manual(values=colors)+
    theme(plot.title = element_text(hjust=0.5, size=plot_text_size),
          axis.title.y = element_blank(),
          axis.text.y  = element_blank(),
          axis.title.x = element_text(size=plot_text_size),
          axis.text.x  = element_text(size=plot_text_size),
          legend.position = 'bottom',
          legend.title=element_blank(),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=plot_text_size),
          strip.text = element_text(size=plot_text_size))
  
  ggsave(paste0('../../viz/ecdf_', c, '.png'),plot,height=10, width=10, units='in', dpi=600)
}
if (regions[1] != 'all'){
  plot_localKGE <- ggarrange(kge_plot_list[[1]],
                         kge_plot_list[[2]],
                         kge_plot_list[[3]], ncol =3, nrow =1, common.legend = T, legend = 'bottom')
  ggsave(paste0('../../viz/ecdf_local.pdf'),plot_localKGE,height=20, width=40, units='in', dpi=600)
}else{
  plot_allKGE <- ggarrange(plot, plotr, plota, plotb, ncol =2, nrow =2, common.legend = T, legend = 'bottom')
  ggsave(paste0('../../viz/ecdf_', c, '.pdf'),plot_allKGE,height=10, width=10, units='in', dpi=600)
}


