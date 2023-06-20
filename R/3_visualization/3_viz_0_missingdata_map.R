####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####
library(ggpubr)
all_dir <- '_all'
# all_dir <- ''
# stationInfo <- read.csv(paste0('../../data/stationLatLon_selected',all_dir,'.csv')) %>%
#   select(., grdc_no, lon, lat, miss_obs, miss_sm, miss_sc, miss_lwe, area) %>% 
#   mutate(available_obs=100-miss_obs, available_sm=100-miss_sm,
#          available_sc=100-miss_sc, available_lwe=100-miss_lwe)


outputDir <- paste0('../../viz/missing/')
dir.create(outputDir, showWarnings = F, recursive = T)

stations_xy <- stationInfo %>% select(grdc_no, lat, lon)

myPalette <- colorRampPalette((brewer.pal(9, "RdYlBu")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(0,100), 
                           breaks=c(0,50,100), name='Available data (%)')

miss_map_fun <- function(area, miss_var, name, countries, wr_countries, L){
  # Missing obs map ####

  stations <- read.csv(paste0('../../data/stationLatLon_selected_all.csv'))
  stationInfo <- read.csv(paste0('../../data/stationLatLon_selected_all.csv')) %>%
    # filter(country %in% countries) %>% 
    select(., grdc_no, lon, lat, miss_obs, miss_sm, miss_sc, miss_lwe, area) %>%
    mutate(available_obs=100-miss_obs, available_sm=100-miss_sm,
           available_sc=100-miss_sc, available_lwe=100-miss_lwe) 
  
  # countries <- c('AU')
  # wr_countries <- c('Australia')
  if (wr_countries[1] == 'world'){
    stationInfo <- read.csv(paste0('../../data/stationLatLon_selected_all.csv')) %>%
      select(., grdc_no, lon, lat, miss_obs, miss_sm, miss_sc, miss_lwe, area) %>%
      mutate(available_obs=100-miss_obs, available_sm=100-miss_sm,
             available_sc=100-miss_sc, available_lwe=100-miss_lwe) 
    wr<- map_data("world")
  }else{
    stationInfo <- read.csv(paste0('../../data/stationLatLon_selected_all.csv')) %>%
      filter(country %in% countries) %>% 
      select(., grdc_no, lon, lat, miss_obs, miss_sm, miss_sc, miss_lwe, area) %>%
      mutate(available_obs=100-miss_obs, available_sm=100-miss_sm,
             available_sc=100-miss_sc, available_lwe=100-miss_lwe) 
    
    wr<- map_data("world") %>% filter(region %in% wr_countries)
    }
  
  name_index <- which(names(stationInfo)== miss_var)
  stationInfo <- stationInfo %>% mutate(available = .[[name_index]])
  
  missing_map <- ggplot() +
    geom_map(aes(map_id = region), map = wr, data = wr, color = 'white', fill = 'gray') +
    expand_limits(x = wr$long, y = wr$lat) +
    theme_map()+
    xlim(area[1],area[2])+
    ylim(area[3],area[4])+
    geom_point(stationInfo, mapping = aes(x = lon, y = lat,  
                                          fill=available,
                                          size = area),
               color='black', pch=21, alpha=0.8, show.legend = L) +
    sc+
    theme(legend.title = element_text(size=20),
          legend.text = element_text(size = 16),
          legend.direction = 'horizontal',
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())+
    scale_size(name=expression(paste("Upstream area ", "(km"^"2",")")),
               breaks=c(10000,100000,500000,1000000,4680000),
               labels=c('asd','10 000 < A < 100 000',
                        '100 000 < A < 500 000', '500 000 < A < 1 000 000 ',
                        '1 000 000 < A < 4 680 000'),
               range=c(2,8))+
    guides(size=guide_legend(direction='vertical'))
  
  missing_map
  
  # ggsave(paste0(outputDir,'map_miss_',name,'.png'), missing_map, height=7, width=14, units='in', dpi=600, bg='white')
  return (missing_map)
}
  
# missing_map_obs_USCA <- miss_map_fun(c(-165.00110,  -52.32320,   24.94930,   83.33621)
#                                 , 'available_obs', paste0('obs_all_stations_',c), c('US', 'CA'), c('USA', 'Canada'), F)
# missing_map_obs_AU <- miss_map_fun(c(110.2460938,	155.2249543, -55.3228175,	-9.0882278	)
#                                 , 'available_obs', paste0('obs_all_stations_',c), c('AU'), c('Australia'), T)
# missing_obs <- ggarrange(missing_map_obs_USCA, missing_map_obs_AU)
# ggsave(paste0(outputDir,'map_miss_obs.png'), missing_obs, height=7, width=14, units='in', dpi=600, bg='white')
# 
# missing_map_lwe_USCA <- miss_map_fun(c(-165.00110,  -52.32320,   24.94930,   83.33621)
#                                      , 'available_lwe', paste0('obs_all_stations_',c), c('US', 'CA'), c('USA', 'Canada'), F)
# missing_map_lwe_AU <- miss_map_fun(c(110.2460938,	155.2249543, -55.3228175,	-9.0882278	)
#                                    , 'available_lwe', paste0('obs_all_stations_',c), c('AU'), c('Australia'), T)
# missing_obs <- ggarrange(missing_map_lwe_USCA, missing_map_lwe_AU)
# ggsave(paste0(outputDir,'map_miss_lwe.png'), missing_obs, height=7, width=14, units='in', dpi=600, bg='white')
# 
# missing_map_sm_USCA <- miss_map_fun(c(-165.00110,  -52.32320,   24.94930,   83.33621)
#                                      , 'available_sm', paste0('obs_all_stations_',c), c('US', 'CA'), c('USA', 'Canada'), F)
# missing_map_sm_AU <- miss_map_fun(c(110.2460938,	155.2249543, -55.3228175,	-9.0882278	)
#                                    , 'available_sm', paste0('obs_all_stations_',c), c('AU'), c('Australia'), T)
# missing_obs <- ggarrange(missing_map_sm_USCA, missing_map_sm_AU)
# ggsave(paste0(outputDir,'map_miss_sm.png'), missing_obs, height=7, width=14, units='in', dpi=600, bg='white')
# 
# missing_map_sc_USCA <- miss_map_fun(c(-165.00110,  -52.32320,   24.94930,   83.33621)
#                                     , 'available_sc', paste0('obs_all_stations_',c), c('US', 'CA'), c('USA', 'Canada'), F)
# missing_map_sc_AU <- miss_map_fun(c(110.2460938,	155.2249543, -55.3228175,	-9.0882278	)
#                                   , 'available_sc', paste0('obs_all_stations_',c), c('AU'), c('Australia'), T)
# missing_obs <- ggarrange(missing_map_sc_USCA, missing_map_sc_AU)
# ggsave(paste0(outputDir,'map_miss_sc.png'), missing_obs, height=7, width=14, units='in', dpi=600, bg='white')


missing_map_obs_world <- miss_map_fun(c(-180,	180, -55,	75	)
                                      , 'available_obs', paste0('obs_all_stations_',c), c('all'), c('world'), T)
ggsave(paste0(outputDir,'map_miss_obs.pdf'), missing_map_obs_world, height=7, width=14, units='in', dpi=600, bg='white')

missing_map_lwe_world <- miss_map_fun(c(-180,	180, -55,	75	)
                                  , 'available_lwe', paste0('obs_all_stations_',c), c('all'), c('world'), T)
ggsave(paste0(outputDir,'map_miss_lwe.pdf'), missing_map_lwe_world, height=7, width=14, units='in', dpi=600, bg='white')

missing_map_sm_world <- miss_map_fun(c(-180,	180, -55,	75	)
                                      , 'available_sm', paste0('obs_all_stations_',c), c('all'), c('world'), T)
ggsave(paste0(outputDir,'map_miss_sm.pdf'), missing_map_sm_world, height=7, width=14, units='in', dpi=600, bg='white')
missing_map_sc_world <- miss_map_fun(c(-180,	180, -55,	75	)
                                      , 'available_sc', paste0('obs_all_stations_',c), c('all'), c('world'), T)
ggsave(paste0(outputDir,'map_miss_sc.pdf'), missing_map_sc_world, height=7, width=14, units='in', dpi=600, bg='white')


# miss_map_fun(area, 'available_sm', paste0('sm_all_stations_', c))
# miss_map_fun(area, 'available_lwe', paste0('lwe_all_stations_', c))
# miss_map_fun(area, 'available_sc', paste0('sc_all_stations_', c))
# 
# 
# plot_missing('AU', 'Australia', c(110.2460938,	155.2249543, -55.3228175,	-9.0882278	))
# plot_missing('US', 'USA', c(-165.0011,	-66.9326,24.9493,	70.5904	))
# plot_missing('CA', 'Canada', c(-145.00275,	-52.3231981,	38.6765556,	83.3362128)	)
# # plot_missing('all', '', c(-180,180,	-55,75)	)
# 
# 
# c(-165.0011, -52.3231981, 24.9493,	83.3362128	)








# ####-------------------------------####
# source('fun_0_loadLibrary.R')
# ####-------------------------------####
# all_dir <- '_all'
# # all_dir <- ''
# # stationInfo <- read.csv(paste0('../../data/stationLatLon_selected',all_dir,'.csv')) %>%
# #   select(., grdc_no, lon, lat, miss_obs, miss_sm, miss_sc, miss_lwe, area) %>% 
# #   mutate(available_obs=100-miss_obs, available_sm=100-miss_sm,
# #          available_sc=100-miss_sc, available_lwe=100-miss_lwe)
# 
# plot_missing <- function(c, r, box){
#   c <- 'all'
#   
#   area <- box
#   area <- c(-165.0011, -52.3231981, 24.9493,	83.3362128	)
# 
#   stationInfo <- read.csv(paste0('../../data/stationLatLon_selected_',c,'_with_missing.csv')) %>%
#     filter(country %in% c('US', 'CA')) %>% 
#     select(., grdc_no, lon, lat, miss_obs, miss_sm, miss_sc, miss_lwe, area) %>%
#     mutate(available_obs=100-miss_obs, available_sm=100-miss_sm,
#            available_sc=100-miss_sc, available_lwe=100-miss_lwe) 
#   
#   outputDir <- paste0('../../viz/missing/')
#   dir.create(outputDir, showWarnings = F, recursive = T)
#   
#   wr <- map_data("world") %>% filter(region %in% c('USA', 'Canada'))
#   
#   
#   
#   stations_xy <- stationInfo %>% select(grdc_no, lat, lon)
#   
#   myPalette <- colorRampPalette((brewer.pal(9, "RdYlBu")))
#   sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(0,100), 
#                              breaks=c(0,50,100), name='Available data (%)')
#   
#   miss_map_fun <- function(area, miss_var, name){
#     # Missing obs map ####
#     name_index <- which(names(stationInfo)== miss_var)
#     stationInfo <- stationInfo %>% mutate(available = .[[name_index]])
#     
#     missing_map <- ggplot() +
#       geom_map(aes(map_id = region), map = wr, data = wr, color = 'white', fill = 'gray') +
#       expand_limits(x = wr$long, y = wr$lat) +
#       theme_map()+
#       xlim(area[1],area[2])+
#       ylim(area[3],area[4])+
#       geom_point(stationInfo, mapping = aes(x = lon, y = lat,  
#                                             fill=available,
#                                             size = area),
#                  color='black', pch=21, alpha=0.8) +
#       sc+
#       theme(legend.title = element_text(size=20),
#             legend.text = element_text(size = 16),
#             legend.direction = 'horizontal',
#             axis.title.x = element_blank(),
#             axis.title.y = element_blank(),
#             axis.ticks = element_blank(),
#             panel.grid = element_blank())+
#       scale_size(name=expression(paste("Upstream area ", "(km"^"2",")")),
#                  breaks=c(10000,100000,500000,1000000,4680000),
#                  labels=c('asd','10 000 < A < 100 000',
#                           '100 000 < A < 500 000', '500 000 < A < 1 000 000 ',
#                           '1 000 000 < A < 4 680 000'),
#                  range=c(2,8))+
#       guides(size=guide_legend(direction='vertical'))
#       
#     missing_map
#     
#     ggsave(paste0(outputDir,'map_miss_',name,'.png'), missing_map, height=7, width=14, units='in', dpi=600, bg='white')
#     
#   }
#   
#   missing_map_obs <- miss_map_fun(area, 'available_obs', paste0('obs_all_stations_',c))
#   miss_map_fun(area, 'available_sm', paste0('sm_all_stations_', c))
#   miss_map_fun(area, 'available_lwe', paste0('lwe_all_stations_', c))
#   miss_map_fun(area, 'available_sc', paste0('sc_all_stations_', c))
#  }
# 
# plot_missing('AU', 'Australia', c(110.2460938,	155.2249543, -55.3228175,	-9.0882278	))
# plot_missing('US', 'USA', c(-165.0011,	-66.9326,24.9493,	70.5904	))
# plot_missing('CA', 'Canada', c(-145.00275,	-52.3231981,	38.6765556,	83.3362128)	)
# # plot_missing('all', '', c(-180,180,	-55,75)	)
# 
# 
# c(-165.0011, -52.3231981, 24.9493,	83.3362128	)
# 
# 
# 
