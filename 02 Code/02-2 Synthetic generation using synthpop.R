#---------------------------------------------------------------------#
# 
# Generate Synthetic Data : Package "synthpop" for simulation 1
# 
#---------------------------------------------------------------------#

# Setting --------------------------------------------------------####
## install packages...
if(!requireNamespace('synthpop')){
  install.packages("synthpop")
}

library(synthpop)

## dir check...
setwd("~Directory") ## working directory

if(!dir.exists("03 Output")) dir.create("03 Output")
if(!dir.exists("03 Output\\Synthetic datasets")) dir.create("03 Output\\Synthetic datasets")
if(!dir.exists("03 Output\\Synthetic datasets\\synthpop")) dir.create("03 Output\\Synthetic datasets\\synthpop")

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
## generate synthetic data ####
syn_smooth <- list()

for(seedNum in 1:sample_num){
  syn_smooth[[seedNum]] <- syn(True_data[sample_index[,seedNum],], method="cart", 
                               smoothing=list(y0="density", y1="density", y2="density"), seed=seedNum, m=m_num)
  
  save(syn_smooth,
       file=paste0("03 Output\\Synthetic datasets\\synthpop\\synthetic results (size ", sample_size, ").rda"))
}


