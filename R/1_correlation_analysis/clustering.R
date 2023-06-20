####-------------------------------####
source('fun_0_loadLibrary.R')
####-------------------------------####

# cluster station gebasseerd op static variables
  # run correlation based on clustering and kge distribution????
  # see what static variables show the best correlation 

# use two clusters since i have two classes that i want to select --> either pcr or pcr sat 
# is better performing and than compare this clustering approach with a correlation method. Loop
# over a lot of different combinations of static variables to see what combination gives the best correlation
# too the pattern visible in the KGE difference plot.

# get station static var values
inputDir <- '../../data/preprocess/pcr_parameters/'
stationInfo <- read.csv(paste0('../../data/stationLatLon_selected_all.csv'))

stationInfo <- stationInfo %>% filter(country == 'AU')

# Open or read pcr_parameters.csv #####
file_dir <- '../../data/predictors/pcr_parameters_all.csv'
if (file.exists(file_dir)){
  stationParameters <- read.csv(file_dir)
}else{
  for (i in 1:length(stationInfo$grdc_no)){
    station_no <- stationInfo$grdc_no[i]
    parameters <- read.csv(paste0(inputDir, 'pcr_parameters_',station_no,'.csv' ))
    if (i == 1){
      stationParameters <- tibble(grdc_no = station_no, parameters[1, ])
    }else{
      stationParameters <- stationParameters %>%  add_row(grdc_no = station_no,parameters[1, ])
    }
  }
  # write.csv(x = stationParameters, file = file_dir, row.names = F)
}

meteo_means <- function(selmonth){
  for (i in 1:length(stationInfo$grdc_no)){
    station_no <- stationInfo$grdc_no[i]
    parameters <- read.csv(paste0('../../data/predictors/pcr_qMeteoStatevars/pcr_qMeteoStatevars_'
                                  ,station_no,'.csv' )) 
    parameters <- parameters %>% mutate(datetime = as.Date(datetime), month = month(datetime)) %>% 
      filter(month == selmonth)
    parameters <- parameters[, c('precipitation', 'temperature' ,'referencePotET')]
    means <- colMeans(parameters)
    if (i == 1){
      stationParameters <- tibble(grdc_no = station_no, precipitation = means[1],
                                  temperature = means[2], referencePotET = means[3])
    }else{
      stationParameters <- stationParameters %>%  
        add_row(grdc_no = station_no, precipitation = means[1],
                temperature = means[2], referencePotET = means[3])
    }
  }
  return(stationParameters)
}

clustering_fun <- function(clust_var){
  # join static parameters and stationInfo
    staticParameters <- inner_join(stationInfo %>% select(grdc_no, lat,lon, area),stationParameters, by = 'grdc_no')

  clustering <- kmeans(staticParameters[, clust_var], center = 2, nstart = 1000, iter.max = 1000)
  
  staticParameters$cluster <-  as.factor(clustering$cluster)
  return (staticParameters[,c('grdc_no', 'cluster')])
}





#### data preparation ####
get_KGE_difference <- function(compare1, compare2, c){
  # 
  stationInfoMissing <- read.csv('../../data/stationLatLon_selected_all_with_missing.csv') %>% 
    select(grdc_no, miss_obs, miss_sm, miss_sc, miss_lwe)
  stationInfo <- read.csv('../../data/stationLatLon_catchAttr.csv') %>% 
    select(grdc_no, lon, lat, area, aridityIdx) %>% inner_join(., stationInfoMissing)
  
  # filePathRF <- paste0('../../data/')
  # setup <- list.files(filePathRF, pattern = 'predictors_')
  #choose runs
  setup <- c('predictors_pcr.csv','predictors_pcr_sat.csv'
              , 'predictors_pcr_sat_add.csv',
              'predictors_sat_meteo.csv', 'predictors_sat_meteo_static.csv')
  
  # regions <- c('AU')
  # c <- 'AU'
  # c <- 'all'
  # regions <- c('all')
  samples <- 5
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
  
  plotData <- plotData %>%
    group_by(setup, grdc_no ) %>% 
    summarise(mean_KGE = mean(KGE)) %>% #na.omit(.) %>% 
    inner_join(., stationInfo, by = 'grdc_no') 
  
  # compare1 <- 'pcr'
  # compare2 <- 'pcr sat'
  pcr <- plotData %>% filter(setup == compare1)
  pcr_sat <- plotData %>% filter(setup == compare2)
  pcrjoin <- inner_join(pcr, pcr_sat, by = 'grdc_no') %>% select(grdc_no, mean_KGE.x, mean_KGE.y) %>% 
    rename(mean_KGE_pcr = mean_KGE.x, mean_KGE_pcrSat = mean_KGE.y)
  
  pcr_diff <- pcrjoin %>% mutate(diff = case_when(mean_KGE_pcr <= mean_KGE_pcrSat
                                       ~ 1,
                                       mean_KGE_pcr > mean_KGE_pcrSat
                                       ~ 2))
  
  return(pcr_diff)
}
pcr1 <- get_KGE_difference('sat meteo', 'sat meteo static', 'all')
pcr2 <- get_KGE_difference('pcr', 'pcr sat', 'all')


pcr3 <- pcr1 %>% filter(diff == 1)
pcr3 %>% filter(mean_KGE_pcr <= mean_KGE_pcrSat)

pcr4 <- pcr2 %>% filter(diff == 1)
pcr5 <- inner_join(pcr3,pcr4, by = 'grdc_no')

pcr6 <- pcr1 %>% filter(diff == 2)
pcr6 %>% filter(mean_KGE_pcrSat > mean_KGE_pcr)


pcr7 <- pcr2 %>% filter(diff == 2)
pcr8 <- inner_join(pcr6,pcr7, by = 'grdc_no') %>% select(grdc_no, mean_KGE.x, mean_KGE.y)


corrs[which.max(corrs)]
corrs[which.min(corrs)]

corrs<-c()
grid <- expand.grid(col_names,col_names,col_names, col_names)
count <- 1
for (i in 1:nrow(grid)){
  var1 <- as.character(grid$Var1[i])
  var2 <- as.character(grid$Var2[i])
  var3 <- as.character(grid$Var3[i])
  var4 <- as.character(grid$Var4[i])
  if (var1 != var2 & var1 != var3 & var1!=var4 &var2!=var3 &var2 != var4 & var3 != var4){
    cluster <- clustering_fun(c(var1, var2, var3, var4))
    new <- inner_join(pcr,cluster, by ='grdc_no')
    corr <- cor(new$diff, as.numeric(new$cluster), use = "complete.obs")
    corrs <- c(corrs, corr)
    names(corrs)[count] <- paste0(var1, '_', var2, '_', var3, '_', var4)
    count <- count +1
  }
}
cluster <- clustering_fun(c('specificYield', 'poreSize2', 'poreSize1', 'bankArea' ))
new <- inner_join(pcr,cluster, by ='grdc_no')
corr <- cor.test(new$diff, as.numeric(new$cluster), use = 'complete.obs')


wr <- map_data("world")
area <- c(-180,	180, -55,	75	)
area<- c(110.2460938,	155.2249543, -55.3228175,	-9.0882278	)
stationInfo <- stationInfo %>% left_join(cluster)

a <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, color = 'white', fill = 'gray') +
  expand_limits(x = wr$long, y = wr$lat) +
  theme_map()+
  xlim(area[1],area[2])+
  ylim(area[3],area[4])+
  geom_point(stationInfo, mapping = aes(x = lon, y = lat,
                                                      fill=cluster),
             color='black', pch=21, size = 2.5) +
  scale_fill_brewer(palette='RdYlBu', guide = guide_legend(reverse=TRUE), name='')

b <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, color = 'white', fill = 'gray') +
  expand_limits(x = wr$long, y = wr$lat) +
  theme_map()+
  xlim(area[1],area[2])+
  ylim(area[3],area[4])+
  geom_point(pcr, mapping = aes(x = lon, y = lat,
                                        fill=as.factor(diff)),
             color='black', pch=21, size = 2.5) +
  scale_fill_brewer(palette='RdYlBu', guide = guide_legend(reverse=TRUE), name='')


ggarrange(a,b)

library(caret)
recall()

lvs <- c("Relevant", "Irrelevant")
tbl_2_1_pred <- factor(rep(lvs, times = c(42, 58)), levels = lvs)
tbl_2_1_truth <- factor(c(rep(lvs, times = c(30, 12)),
                          rep(lvs, times = c(30, 28))),
                        levels = lvs)
test <- inner_join(pcr, cluster)
tbl_2_1 <- table(test$diff, test$cluster)
caret::confusionMatrix(tbl_2_1)

grid$Var1
#meteo vars for months june december means
# satellite vars means for the same periods
cluster <- clustering_fun(col_names)
new <- inner_join(pcr,cluster, by ='grdc_no')
corr <- cor(new$diff, as.numeric(new$cluster), use = "complete.obs")


library(factoextra)

a <- staticParameters %>% select(-grdc_no , -cluster, -lat, -lon, -datetime, -area)
res.pca <- prcomp(a,
             center = TRUE,
             scale. = TRUE)
attributes(res.pca)
fviz_eig(res.pca)

eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 


fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
