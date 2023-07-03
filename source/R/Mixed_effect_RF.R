library(LongituRF)
data <- read.csv('../../RF/0_rf_input/subsample_1_all/train_MRF_table_predictors_sat_meteo_static.csv')

dateInts <- tibble(dateInt = 1:length(unique(data$datetime)), 
                   datetime = sort(unique(data$datetime)))

data <- data %>% arrange(id, datetime) %>% inner_join(dateInts, by = 'datetime')


ids <- sample(data$id, 1)
data5 <- data %>% filter(id %in% ids)  %>% select(-datetime)
data5 <- data5[1:10, 27:34]
length(unique(data5$id))



smerf <- LongituRF::MERF(X = as.matrix(data5[, 28:31]), Y = as.vector(data5$obs),
                         id=data5$id, Z = as.matrix(data5[, 32]), 
                         mtry = 1, 
                         ntree = 10, 
                         iter = 1,
                         time = data5$dateInt,
                         sto = 'none'
)


smerf$random_effects # are the predicted random effects for each individual.
smerf$omega # are the predicted stochastic processes.
plot(smerf$Vraisemblance) # evolution of the log-likelihood.
smerf$OOB # OOB error at each iteration.













