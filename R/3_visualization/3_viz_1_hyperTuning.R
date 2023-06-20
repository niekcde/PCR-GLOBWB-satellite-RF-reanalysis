####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####
library('ggh4x')
outputDir <- paste0('../../viz/')
dir.create(outputDir, showWarnings = F, recursive = T)

sc <- scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#CC79A7"), 
                         name='Subsample: ')

countries <- c('AU', 'CA', 'US','all')


outputDir <- paste0('../../viz/tuningPlots/')
dir.create(outputDir, showWarnings = F, recursive = T)
#### tuning mtry ####

for (country in countries){
  if (country == 'all'){
    setup <- c('predictors_pcr','predictors_pcr_sat', 'predictors_pcr_sat_add',
               'predictors_sat_meteo', 'predictors_sat_meteo_static')
  }else{
    setup <- c('predictors_pcr_sat_lagged_4', 'predictors_pcr_sat_lagged_12',
               'predictors_sat_meteo_lagged_4', 'predictors_sat_meteo_lagged_12',
               'predictors_pcr_sat', 'predictors_pcr', 'predictors_sat_meteo')
  }
  mtryTuningList <- list()
  mtryPlotList <- list()
  plotData <- list()
  for(i in 1:length(setup)){
    for(subsample in 1:5){
        
        tuning_dir <- paste0('../../RF/1_tune/subsample_',subsample,'_',country,'/')
        
        mtry_file <- read.csv(paste0(tuning_dir, 'hyper_grid_',setup[i],'_mtry_ntrees.csv'))
        # mtry_one <- read.csv(paste0(tuning_dir, 'hyper_grid_',setup[i],'_mtry_unit.csv'))
        
        mtry_tuning <- mtry_file %>% arrange(.,mtry) %>% 
          mutate(.,subsample=factor(subsample))
        mtryTuningList[[subsample]] <- mtry_tuning
    }
        
    all_mtry_setup <- do.call(rbind, mtryTuningList)  
    mtryPlotData <- all_mtry_setup %>% pivot_longer(.,OOB_RMSE)
    mtryPlotData <- mtryPlotData %>% select(-ntrees)
    mtryPlotData['setup'] <- gsub(pattern = '_',replacement = ' ',  str_sub(setup[i], start = 12))
    
    plotData[[i]] <- mtryPlotData
    
    plot_text <- 45
    if (setup[i] %in% c('predictors_sat_meteo', 'predictors_sat_meteo_static')){
      y <- c( 0.0004, 0.0007)
    }else{y <- c( 0.0003, 0.0004)}
    if (setup[i] %in% c('predictors_pcr_sat', 'predictors_pcr_sat_add', 'predictors_sat_meteo_static')){
      axisTitleY = element_blank()
      axisTextY  = element_blank()
    }else{
      axisTitleY = element_text(size=plot_text)
      axisTextY  = element_text(size=plot_text)
    }
    if (setup[i] == 'predictors_sat_meteo'){
    minorBreaks <- c(3,3.25,3.5,3.75,4)
    Breaks <- c(3,3,5,4)
    }else{
      minorBreaks <- c(5,10,15,20,15,30)
      Breaks <- c(10,20,30)
    }
    mtryPlot <- ggplot(mtryPlotData, aes(x=mtry,y=value, group=subsample, color=subsample))+
          geom_line(linewidth=2.5)+
          geom_point(aes(shape=subsample), size=4.5, alpha = 1, show.legend = F) +
          sc+
          # ylim(y[1], y[2])+
          scale_x_continuous(minor_breaks = minorBreaks, breaks = Breaks)+
          ylab('OOB RMSE (m/d)\n')+
          labs(title=paste0(setup[i]))+
          # theme_minimal()+
          theme(plot.title = element_text(hjust = 0.5, size=plot_text, face='bold'),
                axis.title.y = axisTitleY,
                axis.text.y  = axisTextY,
                axis.title.x = element_text(size=plot_text),
                axis.text.x  = element_text(size=plot_text),
                
                legend.title=element_blank(),
                legend.key.size = unit(3, 'cm'),
                legend.text = element_text(size=55))
  
    mtryPlotList[[i]] <- mtryPlot
      
      # ggsave(paste0(outputDir,paste0('tuningPlot_',country,'_',setup[i],'.png')), mtryPlot, height=8, width=10, units='in', dpi=600)
  }
  
  # a <- ggarrange(mtryPlotList[[1]], mtryPlotList[[2]], mtryPlotList[[3]], ncol =3, nrow = 1, legend = 'none', widths = c(1,1,1))
  # empty <- tibble()
  # blank <- ggplot(empty) + geom_blank() + theme_minimal()
  # 
  # b <- ggarrange(blank, mtryPlotList[[4]], mtryPlotList[[5]],blank, ncol =4, nrow = 1, widths = c(1,2,2,1), legend = 'bottom',
  #                common.legend = T)
  # 
  # c <- ggarrange(a, b , ncol = 1, nrow =2)
  # 
  # ggsave(paste0(outputDir, paste0('tuningPlot_',country,'.pdf')), c, height=40, width=40,
  #        units='in', dpi=550, limitsize = F)
  
  

  data <- do.call(rbind, plotData)
  
  blank <- ggplot(empty) + geom_blank() + theme_minimal()
  if (country =='all'){
  mtryPlot1 <- ggplot(data %>% filter(setup %in% c('pcr', 'pcr sat', 'pcr sat add'))
                     , aes(x=mtry,y=value, group=subsample, color=subsample))+
    facet_wrap(vars(setup))+
    geom_line(linewidth=2.5)+
    geom_point(aes(shape=subsample), size=6.5, alpha = 1, show.legend = F) +
    sc+
    # ylim(y[1], y[2])+
    scale_x_continuous(minor_breaks = minorBreaks, breaks = c(5,10,15,20,25,30), limits = c(2,35))+
    ylab('OOB RMSE (m/d)\n')+
    # labs(title=paste0(setup[i]))+
    # theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, size=plot_text, face='bold'),
          axis.title.y = element_text(size=plot_text),
          axis.text.y  = element_text(size=plot_text),
          axis.title.x = element_text(size=plot_text),
          axis.text.x  = element_text(size=plot_text),
          strip.text.x = element_text(size = plot_text),
          legend.title=element_blank(),
          legend.key.size = unit(5, 'cm'),
          legend.text = element_text(size=55))
  
  mtryPlot2 <- ggplot(data %>% filter(setup %in% c('sat meteo', 'sat meteo static'))
                      , aes(x=mtry,y=value, group=subsample, color=subsample))+
    facet_wrap(vars(setup), scales = 'free_x')+
    geom_line(linewidth=2.5)+
    geom_point(aes(shape=subsample), size=6.5, alpha = 1, show.legend = F) +
    sc+
    # ylim(y[1], y[2])+
    # scale_x_continuous(minor_breaks = minorBreaks, breaks = Breaks)+
    ylab('OOB RMSE (m/d)\n')+
    theme(plot.title = element_text(hjust = 0.5, size=plot_text, face='bold'),
          axis.title.y = element_text(size=plot_text),
          axis.text.y  = element_text(size=plot_text),
          axis.title.x = element_text(size=plot_text),
          axis.text.x  = element_text(size=plot_text),
          strip.text.x = element_text(size = plot_text),
          legend.title=element_blank(),
          legend.key.size = unit(5, 'cm'),
          legend.text = element_text(size=55)) +
    facetted_pos_scales(x = list(
      setup == "sat_meteo" ~ scale_x_continuous(minor_breaks = c(3,3.25,3.5,3.75,4), 
                                                breaks = c(3,3.5,4)),
      setup == "sat_meteo_static" ~ scale_x_continuous(minor_breaks = c(5,10,15,20,25,30),
                                                       breaks = c(10,20,30), limits = c(2,35))
      ))
  
  
  b <- ggarrange(blank, mtryPlot2,blank, ncol =3, nrow = 1, widths = c(1,4,1), legend = 'none')
  
  c <- ggarrange(mtryPlot1, b , ncol = 1, nrow =2, legend = 'bottom', common.legend = T)
  
  ggsave(paste0(outputDir, paste0('tuningPlot_',country,'.pdf')), c, height=40, width=40,
         units='in', dpi=550, limitsize = F)
  }else{
    data$setup <- factor(data$setup, levels = c('pcr', 'pcr sat', 
                                                'pcr sat lagged 4', 'pcr sat lagged 12',
                                                'sat meteo', 'sat meteo lagged 4', 'sat meteo lagged 12'))
    if (country == 'CA'){
      minorBreaksY <- seq(0.000275, 0.0004, 0.000025)
      BreaksY <- seq(0.000275, 0.0004, 0.00005)
      limitsY <- c(0.00027,0.000376)
      
      minorBreaksY3 <- seq(0.00045, 0.00075, 0.000025)
      BreaksY3 <- seq(0.00045, 0.00075, 0.00005)
      limitsY3 <- c(0.00045,0.00075)
    }else if (country == 'US'){
      minorBreaksY <- seq(0.000225, 0.0004, 0.000025)
      BreaksY <- seq(0.00025, 0.0004, 0.00005)
      limitsY <- c(0.00025,0.00035)
      
      minorBreaksY3 <- seq(0.00045, 0.00075, 0.000025)
      BreaksY3      <- seq(0.00045, 0.00075, 0.00005)
      limitsY3      <- c(0.00045,0.00060)
    }else if (country == 'AU'){
      minorBreaksY <- seq(0.000225, 0.00045, 0.000025)
      BreaksY <- seq(0.00025, 0.00045, 0.00005)
      limitsY <- c(0.000225,0.00045)
      
      minorBreaksY3 <-  minorBreaksY
      BreaksY3      <-  BreaksY 
      limitsY3      <-  limitsY 
    }
    minorBreaksX <- c(5,10,15,20,25,30,35)
    BreaksX <- c(10,20,30)
    limitsX <- c(2,35)
    
    mtryPlot1 <- ggplot(data %>% filter(setup %in% c('pcr', 'pcr sat'))
                        , aes(x=mtry,y=value, group=subsample, color=subsample))+
      facet_wrap(vars(setup))+
      geom_line(linewidth=2.5)+
      geom_point(aes(shape=subsample), size=6.5, alpha = 1, show.legend = F) +
      sc+
      scale_x_continuous(minor_breaks = minorBreaksX, breaks = BreaksX, limits = limitsX)+
      scale_y_continuous(minor_breaks = minorBreaksY, breaks = BreaksY, limits = limitsY)+
      ylab('OOB RMSE (m/d)\n')+
      theme(plot.title = element_text(hjust = 0.5, size=plot_text, face='bold'),
            axis.title.y = element_text(size=plot_text),
            axis.text.y  = element_text(size=plot_text),
            axis.title.x = element_text(size=plot_text),
            axis.text.x  = element_text(size=plot_text),
            strip.text.x = element_text(size = plot_text),
            legend.title=element_blank(),
            legend.key.size = unit(5, 'cm'),
            legend.text = element_text(size=55))
    
    mtryPlot2 <- ggplot(data %>% filter(setup %in% c('pcr sat lagged 4', 'pcr sat lagged 12'))
                        , aes(x=mtry,y=value, group=subsample, color=subsample))+
      facet_wrap(vars(setup), scales = 'free_x')+
      geom_line(linewidth=2.5)+
      geom_point(aes(shape=subsample), size=6.5, alpha = 1, show.legend = F) +
      sc+
      scale_x_continuous(minor_breaks = minorBreaksX, breaks = BreaksX, limits = limitsX)+
      scale_y_continuous(minor_breaks = minorBreaksY, breaks = BreaksY, limits = limitsY)+
      ylab('OOB RMSE (m/d)\n')+
      theme(plot.title = element_text(hjust = 0.5, size=plot_text, face='bold'),
            axis.title.y = element_text(size=plot_text),
            axis.text.y  = element_text(size=plot_text),
            axis.title.x = element_text(size=plot_text),
            axis.text.x  = element_text(size=plot_text),
            strip.text.x = element_text(size = plot_text),
            legend.title=element_blank(),
            legend.key.size = unit(5, 'cm'),
            legend.text = element_text(size=55))

    mtryPlot3 <- ggplot(data %>% filter(setup %in% c('sat meteo', 'sat meteo lagged 4', 'sat meteo lagged 12'))
                             , aes(x=mtry,y=value, group=subsample, color=subsample))+
      facet_wrap(vars(setup), scales = 'free_x')+
      geom_line(linewidth=2.5)+
      geom_point(aes(shape=subsample), size=6.5, alpha = 1, show.legend = F) +
      sc+
      scale_y_continuous(minor_breaks = minorBreaksY3, breaks = BreaksY3, limits = limitsY3)+
      ylab('OOB RMSE (m/d)\n')+
      theme(plot.title = element_text(hjust = 0.5, size=plot_text, face='bold'),
            axis.title.y = element_text(size=plot_text),
            axis.text.y  = element_text(size=plot_text),
            axis.title.x = element_text(size=plot_text),
            axis.text.x  = element_text(size=plot_text),
            strip.text.x = element_text(size = plot_text),
            legend.title=element_blank(),
            legend.key.size = unit(5, 'cm'),
            legend.text = element_text(size=55)) +
      facetted_pos_scales(x = list(
        setup == "sat meteo" ~ scale_x_continuous(minor_breaks = c(3,3.25,3.5,3.75,4), 
                                                  breaks = c(3,3.5,4)),
        setup == "sat meteo lagged 4" ~ scale_x_continuous(minor_breaks = minorBreaksX,
                                                         breaks = BreaksX, limits = limitsX),
        setup == "sat meteo lagged 12" ~ scale_x_continuous(minor_breaks = minorBreaksX,
                                                           breaks = BreaksX, limits = limitsX)
      ))
    
    a <- ggarrange(blank, mtryPlot1, blank, ncol =3, nrow = 1, widths = c(1,4,1), legend = 'none')
    b <- ggarrange(blank, mtryPlot2, blank, ncol =3, nrow = 1, widths = c(1,4,1), legend = 'none')
    
    c <- ggarrange(a, b, mtryPlot3 , ncol = 1, nrow =3, legend = 'bottom', common.legend = T)
    
    ggsave(paste0(outputDir, paste0('tuningPlot_',country,'.pdf')), c, height=40, width=40,
           units='in', dpi=550, limitsize = F)
  }
  
}