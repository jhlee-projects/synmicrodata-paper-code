#---------------------------------------------------------------------#
# 
# Generate Synthetic Data : imputation missing for simulation 2
# 
#---------------------------------------------------------------------#

## Setting --------------------------------------------------------####
### install packages...
if(!requireNamespace('synMicrodata')){
  install.packages("synMicrodata")
}

library(synMicrodata)

### dir check...
setwd("~Directory") ## working directory

if(!dir.exists("03 Output")) dir.create("03 Output")
if(!dir.exists("03 Output\\Synthetic datasets")) dir.create("03 Output\\Synthetic datasets")
if(!dir.exists("03 Output\\Synthetic datasets\\with missing(impute)")) dir.create("03 Output\\Synthetic datasets\\with missing(impute)")

### initial setting...
rm(list=ls()); gc()

sample_num <- 500 ## number of iteration
sample_size <- 1000 ## sample size
m_num <- 10 ## number of synthetic data

### data load...
load("01 Data\\Simulation datasets\\simulation dataset (population).rda") ## population data
load(paste0("01 Data\\Simulation datasets\\sample index (size ", sample_size, ").rda")) ## sample index
load(paste0("01 Data\\Simulation datasets\\miss index (size ", sample_size, ").rda")) ## missing index

### variable names
x_var <- paste0("x", 1:5)
y_var <- paste0("y", 0:2)


## Generate synthetic datsets -------------------------------------####

### for synMicrodata
n_burnin <- 5000 ## burn-in size
interval_btw_syn <- 1000 ## interval

### generate synthetic data
syn_impu <- list()

for(seedNum in 1:sample_num){
  ## missing dataset...
  miss_data <- True_data[sample_index[,seedNum],]
  miss_data[miss_index[,,seedNum]==1] <- NA
  
  ## prepare multipleSyn
  data_obj = readData(Y_input=miss_data[,y_var], 
                      X_input=miss_data[,x_var], RandomSeed=seedNum)
  
  model_obj = createModel(data_obj, max_R_S_K=c(20,40,20))
  
  syn_impu[[seedNum]] = multipleSyn(data_obj, model_obj, n_burnin, m_num, interval_btw_syn, show_iter=FALSE)
  message(paste0("Sample ", seedNum, " End: ", Sys.time()))
  
  save(syn_impu,
       file=paste0("03 Output\\Synthetic datasets\\with missing(impute)\\synthetic results (size ", sample_size, ").rda"))
}

