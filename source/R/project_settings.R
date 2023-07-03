# Project settings
# choose which products are included, select period and select project regions
# configurations?

# R project file should be loaded in the source/R/ folder

# Period
startDate    <- '2002-04-01'
endDate      <- '2019-12-01'
endDateDaily <- '2019-12-31'

# number of available cores on computer
cores <- 4

satelliteProducts <- c('lwe', 'sc', 'sm')

# RF constant settings
num.threads   <- 48 
min.node.size <- 5

# different samples for validation
samples <- 5