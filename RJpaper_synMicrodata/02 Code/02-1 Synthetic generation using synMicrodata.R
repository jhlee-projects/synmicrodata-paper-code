#---------------------------------------------------------------------#
# 
# Generate Synthetic Data : Package "synMicrodata" for simulation 1
# 
#---------------------------------------------------------------------#

# Setting --------------------------------------------------------####
## install packages...
if(!requireNamespace('synMicrodata')){
  install.packages("synMicrodata")
}

library(synMicrodata)

## dir check...
setwd("~Directory") ## working directory

if(!dir.exists("03 Output")) dir.create("03 Output")
if(!dir.exists("03 Output\\Synthetic datasets")) dir.create("03 Output\\Synthetic datasets")
if(!dir.exists("03 Output\\Synthetic datasets\\synMicrodata")) dir.create("03 Output\\Synthetic datasets\\synMicrodata")

## initial setting...
rm(list=ls())

sample_num <- 500 ## number of iteration
sample_size <- 1000 ## sample size
m_num <- 10 ## number of synthetic data

## data load...
load("01 Data\\Simulation datasets\\simulation dataset (population).rda") ## population data
load(paste0("01 Data\\Simulation datasets\\sample index (size ", sample_size, ").rda")) ## sample index

## variable names
x_var <- paste0("x", 1:5)
y_var <- paste0("y", 0:2)


# Generate synthetic datsets -------------------------------------####
## for synMicrodata
n_burnin <- 5000 ## burn-in size
interval_btw_syn <- 1000 ## interval

## generate synthetic data ####
syn_micro <- list()

for(seedNum in 1:sample_num){
  ## prepare multipleSyn
  data_obj = readData(Y_input=True_data[sample_index[,seedNum],y_var], 
                      X_input=True_data[sample_index[,seedNum],x_var], RandomSeed=seedNum)
  
  model_obj = createModel(data_obj, max_R_S_K=c(20,40,20))
  
  syn_micro[[seedNum]] = multipleSyn(data_obj, model_obj, n_burnin, m_num, interval_btw_syn, show_iter=FALSE)
  message(paste0("Sample ", seedNum, " End: ", Sys.time()))
  
  save(syn_micro,
       file=paste0("03 Output\\Synthetic datasets\\synMicrodata\\synthetic results (size ", sample_size, ").rda"))
}
