####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####
library(ggpubr)


outputDir <- paste0('../../viz/missing/')
dir.create(outputDir, showWarnings = F, recursive = T)

stations_xy <- stationInfo %>% select(grdc_no, lat, lon)

myPalette <- colorRampPalette((brewer.pal(9, "RdYlBu")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(0,100), 
                           breaks=c(0,50,100), name='Available data (%)')

miss_map_fun <- function(area, region, L){
  
  stationInfo <- read.csv(paste0('../../data/stationLatLon_selected_',region,'.csv')) %>%
    select(., grdc_no, lon, lat, starts_with('miss'), area) %>%
    mutate(available_obs=100-miss_obs, available_sm=100-miss_sm,
           available_sc=100-miss_sc, available_lwe=100-miss_lwe) 
  
  wr<- map_data("world")
  
  for (missVar in c('obs',satelliteProducts)){
    name_index <- which(names(stationInfo) == paste0('available_',missVar))
    stationInfo <- stationInfo %>% mutate(available = .[[name_index]])
  
    missing_map <- ggplot() +
      geom_map(aes(map_id = region), map = wr, data = wr, color = 'white', fill = 'gray') +
      expand_limits(x = wr$long, y = wr$lat) +
      theme_map()+
      xlim(area[1],area[2])+
      ylim(area[3],area[4])+
      geom_point(stationInfo, mapping = aes(x = lon, y = lat,  
                                            fill=available,
                                            size = area
                                            ),
                 color='black', pch=21, alpha=0.8, show.legend = L) +
      sc+
      theme(legend.title = element_text(size=20),
            legend.text = element_text(size = 20),
            legend.direction = 'horizontal',
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            )+
      scale_size(name=expression(paste("Upstream area ", "(km"^"2",")")),
                 breaks=c(10000,100000,500000,1000000,4680000),
                 labels=c('asd','10 000 < A < 100 000',
                          '100 000 < A < 500 000', '500 000 < A < 1 000 000 ',
                          '1 000 000 < A < 4 680 000'),
                 range=c(2,8))+
      guides(size=guide_legend(direction='vertical'))
    
    missing_map
    ggsave(paste0(outputDir,'map_miss_',region,'_',missVar,'.pdf'), missing_map, height=7, width=14, units='in', dpi=600, bg='white')
  }
  return (missing_map)
}
  
missing_map_obs_world <- miss_map_fun(c(-180,	180, -55,	75	),'all', T) # change first and second input for different region






