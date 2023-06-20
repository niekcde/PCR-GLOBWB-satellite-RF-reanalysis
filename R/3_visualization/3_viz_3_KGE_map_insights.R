# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")  #with grey
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #with black

####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####
library('ggh4x')

#script that plots map of KGE for uncalibrated and allpredictors, averaged over
#5 test subsamples and insights on which stations have high/low KGE
c <- 'all'

# area <- c(-165.0011,	-66.9326,24.9493,	70.5904	)
area <- c(110.2460938,	155.2249543, -55.3228175,	-9.0882278	)
area <- c(-180,	180, -55,	75	)
samples <- 5
outputDir <- '../../viz/'
dir.create(outputDir, showWarnings = F, recursive = T)

stationInfoMissing <- read.csv('../../data/stationLatLon_selected_all_with_missing.csv') %>% 
  select(grdc_no, miss_obs, miss_sm, miss_sc, miss_lwe)
stationInfo <- read.csv('../../data/stationLatLon_catchAttr.csv') %>% 
  select(grdc_no, lon, lat, area, aridityIdx) %>% inner_join(., stationInfoMissing)

filePathRF <- paste0('../../data/')
setup <- list.files(filePathRF, pattern = 'predictors_')
#choose runs
setup <- c('predictors_pcr.csv','predictors_pcr_sat.csv', 'predictors_pcr_sat_add.csv',
           'predictors_sat_meteo.csv', 'predictors_sat_meteo_static.csv')



regions <- c('AU', 'CA', 'US')
regions <- c('all')
samples <- 1

#### data preparation ####
subsample_KGE_list <- list ()
for(subsample in 1:samples){
  
  rf.eval.uncalibrated <- read.csv(paste0('../../RF/3_validate/subsample_', subsample,'_',c,
                                          '/KGE_predictors_pcr.csv')) %>%
    select(.,grdc_no, KGE) %>%
    mutate(.,setup=factor('uncalibrated')) %>% 
    mutate(.,subsample=factor(subsample)) 

  read_metrics <- function(setup, subsample){
    setup <- str_sub(setup, end = -5)
    setup_factor <- gsub('_', ' ', str_sub(setup, start = 12))
    # print(setup_factor)
    # print(setup)
    rf.eval <- read.csv(paste0('../../RF/3_validate/subsample_', subsample,'_',c,
                               '/KGE_',setup,'.csv')) %>% 
      select(.,grdc_no, KGE_corrected) %>% 
      rename(., KGE=KGE_corrected) %>% 
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
plotData <- do.call(rbind, subsample_KGE_list)

plotData_uncalibrated <- plotData %>% filter(.,setup=='uncalibrated') %>% 
  group_by(grdc_no) %>% 
  summarise(mean_test_KGE_uncalibrated = mean(KGE)) %>% na.omit(.) %>% 
  inner_join(., stationInfo) %>%
  mutate(.,setup=factor('uncalibrated'))

plotData_pcr <- plotData %>% filter(.,setup=='pcr sat add') %>% 
  group_by(grdc_no) %>% 
  summarise(mean_pcr = mean(KGE)) %>% na.omit(.) %>% 
  inner_join(., stationInfo) %>%
  mutate(.,setup=factor('pcr sat add'))

plotData_pcrsat <- plotData %>% filter(.,setup=='pcr sat') %>% 
  group_by(grdc_no) %>% 
  summarise(mean_pcrsat = mean(KGE)) %>% na.omit(.) %>% 
  inner_join(., stationInfo) %>%
  mutate(.,setup=factor('pcr sat'))

plotData_pcr <- plotData_pcr %>% mutate(cat = case_when(
                                                        mean_pcr <= -0.41 ~ 1,
                                                        mean_pcr <= 0 ~ 2,
                                                        mean_pcr <= 0.2 ~ 3,
                                                        mean_pcr <= 0.4 ~ 4,
                                                        mean_pcr <= 0.6 ~ 5,
                                                        mean_pcr <= 0.8 ~ 6,
                                                        mean_pcr <= 0.9 ~ 7,
                                                        mean_pcr <= 1 ~ 8)) %>% 
  mutate(catf = factor(
    case_when(mean_pcr <= -0.41 ~ 'Bad',
              .default = 'Good')))  %>% mutate(mean_pcr2 = mean_pcr + 400)

plotData_pcrsat <- plotData_pcrsat %>% mutate(cat = case_when(
                                                              mean_pcrsat <= -0.41 ~ 1,
                                                              mean_pcrsat <= 0 ~ 2,
                                                              mean_pcrsat <= 0.2 ~ 3,
                                                              mean_pcrsat <= 0.4 ~ 4,
                                                              mean_pcrsat <= 0.6 ~ 5,
                                                              mean_pcrsat <= 0.8 ~ 6,
                                                              mean_pcrsat <= 0.9 ~ 7,
                                                              mean_pcrsat <= 1 ~ 8)) %>% 
  mutate(mean_pcr2 = mean_pcrsat + 400)
  

plotData_pcr_minus_pcrsat <- plotData_pcr %>% mutate(catf = 
                                                       case_when(
                                                         cat < plotData_pcrsat$cat ~ 'pcrsat',
                                                         cat > plotData_pcrsat$cat ~ 'pcr',
                                                         .default = 'equal')) %>% 
  mutate(test = plotData_pcrsat$mean_pcr2- mean_pcr2  )

plotData_pcr_minus_pcrsat <- plotData_pcr %>% mutate(catf = 
                                                       case_when(mean_pcr <= plotData_pcrsat$mean_pcrsat
                                                                 ~ 'sat',
                                                                 mean_pcr > plotData_pcrsat$mean_pcrsat
                                                                 ~ 'pcr',)) 
  


#### plot KGE map uncalibrated, allpredictors ####
wr <- map_data("world")

#-----------KGE--------------#
#set KGE intervals 
breaks=c(-Inf, -0.41, 0, 0.2, 0.4, 0.6, 0.8, 0.9, 1)
labels=c('KGE < -0.41','-0.41 < KGE < 0', '0 < KGE < 0.2','0.2 < KGE < 0.4',
         '0.4 < KGE < 0.6','0.6 < KGE < 0.8','0.8 < KGE < 0.9','0.9 < KGE < 1')

breaks=c(-Inf, -1,-0.5,-0.2, -0.05, 0,0.05, 0.2, 0.4, 0.6, 0.8, 0.9, 1)
labels=c('KGE < -0.41','-0.41 < KGE < 0', '0 < KGE < 0.2','0.2 < KGE < 0.4',
         '0.4 < KGE < 0.6','0.6 < KGE < 0.8','0.8 < KGE < 0.9','0.9 < KGE < 1')

# area <- c(-165.0011,	-66.9326,24.9493,	70.5904	)
# area <- c(110.2460938,	155.2249543, -55.3228175,	-9.0882278	)


ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, color = 'white', fill = 'gray') +
  expand_limits(x = wr$long, y = wr$lat) +
  theme_map()+
  xlim(area[1],area[2])+
  ylim(area[3],area[4])+
  geom_point(plotData_pcr_minus_pcrsat, mapping = aes(x = lon, y = lat, 
                                                      fill=catf),
             color='black', pch=21, size = 2.5) +
  scale_fill_brewer(palette='RdYlBu', guide = guide_legend(reverse=TRUE), name='')+
  labs(title='Uncalibrated PCR-GLOBWB\n') +
  xlab('longitude')+
  ylab('latitude') +
  theme(plot.title = element_text(hjust = 0.5, size=20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())




# 
# KGE_map_uncalibrated <- ggplot() +
#   geom_map(aes(map_id = region), map = wr, data = wr, color = 'white', fill = 'gray') +
#   expand_limits(x = wr$long, y = wr$lat) +
#   theme_map()+
#   xlim(-180,180)+
#   ylim(-55,75)+
#   geom_point(plotData_uncalibrated, mapping = aes(x = lon, y = lat,
#                                       fill=cut(mean_test_KGE_uncalibrated, breaks=breaks, labels=labels)),
#              color='black', pch=21, size = 2.5) +
#   scale_fill_brewer(palette='RdYlBu', guide = guide_legend(reverse=TRUE), name='')+
#   labs(title='Uncalibrated PCR-GLOBWB\n') +
#   xlab('longitude')+
#   ylab('latitude') +
#   theme(plot.title = element_text(hjust = 0.5, size=20),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank())
# 
# KGE_map_allpredictors <- ggplot() +
#   geom_map(
#     data = wg, map = wg,
#     aes(long, lat, map_id = region),
#     color = "white", fill= "grey"
#   ) +
#   theme_map()+
#   xlim(-180,180)+
#   ylim(-55,75)+
#   geom_point(plotData_allpredictors, mapping = aes(x = lon, y = lat,
#                                                   fill=cut(mean_test_KGE_allpredictors, breaks=breaks, labels=labels)),
#              color='black', pch=21, size = 2.5) +
#   scale_fill_brewer(palette='RdYlBu', guide = guide_legend(reverse=TRUE), name='') +
# labs(title="Post-processed - allPredictors\n") +
#   xlab('longitude')+
#   ylab('latitude') +
#   theme(plot.title = element_text(hjust = 0.5, size=20),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank())
# 
# #patch it 
# combined <- ( KGE_map_uncalibrated / KGE_map_allpredictors ) + 
#   plot_layout(guides = "collect", width=c(2,2)) &
#   guides(fill = guide_legend(override.aes = list(size = 7))) &
#   theme(legend.position = 'bottom',
#         legend.title = element_text(size=16),
#         legend.text = element_text(size=16))
# combined
# 
# #save
# ggsave(paste0(outputDir,'map_kge.png'), combined, height=15, width=15, units='in', dpi=600)
# 
# 
# 
# #### scatterplot KGE uncalibrated vs. allpredictors ####
# # scatterData <- merge(plotData_uncalibrated, plotData_allpredictors, by='grdc_no')
# # 
# # ggplot(scatterData) +
# #   geom_point(aes(x = mean_test_KGE_uncalibrated, y = mean_test_KGE_allpredictors)) #+
# #   # xlim(-1,1)+
# #   # ylim(-1,1)
# # rsq <- function (x, y) cor(x, y) ^ 2
# # rsq(scatterData$mean_test_KGE_uncalibrated, scatterData$mean_test_KGE_allpredictors)
# 
# 
# #performance: improved or degraded KGE
# eval_allG <- inner_join(plotData_uncalibrated,plotData_allpredictors, by='grdc_no')
# 
# improvement <- eval_allG[which(eval_allG$mean_test_KGE_uncalibrated < eval_allG$mean_test_KGE_allpredictors),]
# worsening <-   eval_allG[which(eval_allG$mean_test_KGE_uncalibrated > eval_allG$mean_test_KGE_allpredictors),]
# 
# summary(worsening$mean_test_KGE_uncalibrated)
# summary(worsening$mean_test_KGE_allpredictors)
# summary(improvement$mean_test_KGE_uncalibrated)
# summary(improvement$mean_test_KGE_allpredictors)
# 
# summary(worsening$miss.x)
# summary(improvement$miss.x)
# 
# 
# 
# 
