#---------------------------------------------------------------------#
# 
# Generate Synthetic Data : Package "simPop" for simulation 1
# 
#---------------------------------------------------------------------#

# Setting --------------------------------------------------------####
## install packages...
if(!requireNamespace('simPop')){
  install.packages("simPop")
}

library(simPop)


## dir check...
setwd("~Directory") ## working directory

if(!dir.exists("03 Output")) dir.create("03 Output")
if(!dir.exists("03 Output\\Synthetic datasets")) dir.create("03 Output\\Synthetic datasets")
if(!dir.exists("03 Output\\Synthetic datasets\\simPop")) dir.create("03 Output\\Synthetic datasets\\simPop")

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
## for simPop
True_data$id <- 1:nrow(True_data)
True_data$weight <- 1

## generate synthetic data
syn_pop <- list()

for(seedNum in 1:sample_num){
results_list <- list()
  for(m in 1:m_num){
    dat_obj <- specifyInput(data=True_data[sample_index[,seedNum],], hhid="id", strata="x1", weight="weight")
    
    synthP <- simStructure(data=dat_obj, method="direct", basicHHvars=c("x1", "x4"), seed=seedNum + (m*1e+4))
    synthP <- simCategorical(synthP, additional = x_var[-c(1,4)], method="multinom", 
                             regModel="available", nr_cpus=1, seed=100+seedNum + (m*1e+4))
    
    for(i in 1:length(y_var)){
      tmp <- manageSimPopObj(synthP, var=y_var[i], sample=TRUE, set=FALSE)
      breaks <- round(quantile(tmp, seq(0, 1, length=20)), 2)
      
      synthP <- simContinuous(synthP, additional=y_var[i], method="multinom", 
                              breaks=breaks, regModel="available", nr_cpus=1, seed=(i*1000 + seedNum + (m*1e+4)))
      tmp_p <- manageSimPopObj(synthP, var=y_var[i], sample=FALSE, set=FALSE)
      
      synthP <- manageSimPopObj(synthP, var=paste0(y_var[i], "Cat"), sample=TRUE,
                                set=TRUE, values=getCat(x=unlist(tmp), breaks))
      synthP <- manageSimPopObj(synthP, var=paste0(y_var[i], "Cat"), sample=FALSE,
                                set=TRUE, values=getCat(x=unlist(tmp_p), breaks))
    }
    results_list[[m]] <- as.data.frame(popData(synthP))[,c(x_var, y_var)]
  }
  syn_pop[[seedNum]] <- results_list
  
  message(paste0("Sample ", seedNum, " End: ", Sys.time()))
  save(syn_pop,
       file=paste0("03 Output\\Synthetic datasets\\simPop\\synthetic results (size ", sample_size, ").rda"))
}

