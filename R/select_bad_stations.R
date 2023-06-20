####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####


data_dir <- '../../data/KGE_allpredictors_cumulative.csv'
KGE <- read.csv(data_dir)

KGE_bad <- KGE %>% group_by(grdc_no) %>% summarise(mean_KGE=mean(KGE),
                                                   .groups = 'drop') %>% 
  filter(mean_KGE <= -0.42)
write.csv(KGE_bad$grdc_no, '../../data/BAD_stations.csv', row.names = F)

