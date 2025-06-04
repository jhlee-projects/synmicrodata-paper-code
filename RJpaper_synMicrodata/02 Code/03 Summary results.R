#---------------------------------------------------------------------#
# 
# Summary Results
# 
#---------------------------------------------------------------------#

# Setting --------------------------------------------------------####
if(!requireNamespace("dplyr")) install.packages("dplyr")
if(!requireNamespace("colorspace")) install.packages("colorspace")

library(dplyr)
library(colorspace)
library(MASS)

## dir check...
setwd("~Directory") ## working directory

if(!dir.exists("03 Output")) dir.create("03 Output")
if(!dir.exists("03 Output\\Simulation tables")) dir.create("03 Output\\Simulation tables")
if(!dir.exists("03 Output\\Contour plot\\")) dir.create("03 Output\\Contour plot\\")


## initial setting... ####
rm(list=ls())

pop_size <- 1e+06 ## population size
sample_num <- 500 ## number of iteration
sample_size <- 1000 ## sample size
m_num <- 10 ## number of synthetic data

### variable names
x_var <- paste0("x", 1:5)
y_var <- paste0("y", 0:2)

## data & source load... ####
### population data
load("01 Data\\Simulation datasets\\simulation dataset (population).rda") ## population data
load(paste0("01 Data\\Simulation datasets\\sample index (size ", sample_size, ").rda")) ## sample index

### source code: functions for summarizing results
source("02 Code\\__Functions for synthetic results.R")

### synthetic datasets: simulation 1
load(paste0("03 Output\\Synthetic datasets\\synMicrodata\\synthetic results (size ", sample_size, ").rda"))
load(paste0("03 Output\\Synthetic datasets\\synthpop\\synthetic results (size ", sample_size, ").rda"))
load(paste0("03 Output\\Synthetic datasets\\simPop\\synthetic results (size ", sample_size, ").rda"))


### synthetic datasets: simulation 2
load(paste0("01 Data\\Simulation datasets\\miss index (size ", sample_size, ").rda")) ## missing index

load(paste0("03 Output\\Synthetic datasets\\with missing(cc only)\\synthetic results (size ", sample_size, ").rda"))
load(paste0("03 Output\\Synthetic datasets\\with missing(impute)\\synthetic results (size ", sample_size, ").rda"))



# Simulation 1: Summary table ------------------------------------####
## population: True Parameters ####
theta_pop <- True_data %>% within({ ## category to binary (1st-level rate)
  x1 <- ifelse(x1==1, 1, 0)
  x2 <- ifelse(x2==1, 1, 0)
  x3 <- ifelse(x3==1, 1, 0)
  x4 <- ifelse(x4==1, 1, 0)
  x5 <- ifelse(x5==1, 1, 0) 
}) %>% colMeans


## Synthetic results ####
### theta_key: pooled mean & rate (array)
### var_key: pooled variance (array)
### ci_key: confidence interval (list)

### synthetic: synMicrodata (key_word - micro) ####
theta_micro <- var_micro <- 
  matrix(NA, nc=sample_num, nr=8, dimnames = dimnames(c(x_var, y_var)))
ci_micro <- list()

for(seedNum in 1:sample_num){ ### each iteration... 
  ### prepare datasets
  data_micro <- syn_micro[[seedNum]]$synt_data %>% 
    lapply(function(s){
      dd <- within(s, {
        x1 <- ifelse(x1==1, 1, 0)
        x2 <- ifelse(x2==1, 1, 0)
        x3 <- ifelse(x3==1, 1, 0)
        x4 <- ifelse(x4==1, 1, 0)
        x5 <- ifelse(x5==1, 1, 0)
      })
      return(dd[,c(x_var, y_var)])
    } )
  
  
  ### pooled mean & rate
  theta_micro[,seedNum] <- data_micro %>%
    sapply(colMeans) %>% rowMeans

  ### pooled variance calculate
  Bm <- data_micro %>%
    sapply(colMeans) %>% apply(MARGIN=1, var)
  Vm <- data_micro %>%
    sapply(function(dd) apply(dd, MARGIN=2, function(vv) var(vv) * ( 1 - (nrow(dd) / pop_size) ) / nrow(dd) ) ) %>% rowMeans

  var_micro[,seedNum] <- (Bm / m_num) + Vm

  ### confidence interval
  ci_micro[[seedNum]] <- cbind('2.5%' = theta_micro[,seedNum] + qnorm(0.025)* sqrt(var_micro[,seedNum]),
                               '97.5%' = theta_micro[,seedNum] + qnorm(0.975)* sqrt(var_micro[,seedNum]))
}


### synthetic: synthpop (keyword - cart)####
theta_cart <- var_cart <- 
  matrix(NA, nc=sample_num, nr=8, dimnames = dimnames(c(x_var, y_var)))
ci_cart <- list()

for(seedNum in 1:sample_num){ ### each iteration... 
  ### prepare datasets
  data_cart <- syn_smooth[[seedNum]]$syn %>% 
    lapply(function(s) within(s, {
      x1 <- ifelse(x1==1, 1, 0)
      x2 <- ifelse(x2==1, 1, 0)
      x3 <- ifelse(x3==1, 1, 0)
      x4 <- ifelse(x4==1, 1, 0)
      x5 <- ifelse(x5==1, 1, 0)
    }))
  
  ### pooled mean & rate
  theta_cart[,seedNum] <- data_cart %>% 
    sapply(colMeans) %>% rowMeans
  
  ### pooled variance calculate
  Bm <- data_cart %>% 
    sapply(colMeans) %>% apply(MARGIN=1, var)
  Vm <- data_cart %>% 
    sapply(function(dd) apply(dd, MARGIN=2, function(vv) var(vv) * ( 1 - (nrow(dd) / pop_size) ) / nrow(dd) ) ) %>% rowMeans
  
  var_cart[,seedNum] <- (Bm / m_num) + Vm
  
  ### confidence interval
  ci_cart[[seedNum]] <- cbind('2.5%' = theta_cart[,seedNum] + qnorm(0.025)* sqrt(var_cart[,seedNum]),
                               '97.5%' = theta_cart[,seedNum] + qnorm(0.975)* sqrt(var_cart[,seedNum]))
}

### synthetic: simPop ####
theta_sim <- var_sim <- 
  matrix(NA, nc=sample_num, nr=8, dimnames = dimnames(c(x_var, y_var)))
ci_sim <- list()

for(seedNum in 1:sample_num){ ### each iteration... 
  ### prepare datasets
  data_sim <- syn_pop[[seedNum]] %>% 
    lapply(function(s) within(s, {
      x1 <- ifelse(x1==1, 1, 0)
      x2 <- ifelse(x2==1, 1, 0)
      x3 <- ifelse(x3==1, 1, 0)
      x4 <- ifelse(x4==1, 1, 0)
      x5 <- ifelse(x5==1, 1, 0)
    }))
  
  ### pooled mean & rate
  theta_sim[,seedNum] <- data_sim %>% 
    sapply(colMeans) %>% rowMeans
  
  ### pooled variance calculate
  Bm <- data_sim %>% 
    sapply(colMeans) %>% apply(MARGIN=1, var)
  Vm <- data_sim %>% 
    sapply(function(dd) apply(dd, MARGIN=2, function(vv) var(vv) * ( 1 - (nrow(dd) / pop_size) ) / nrow(dd) ) ) %>% rowMeans
  
  var_sim[,seedNum] <- (Bm / m_num) + Vm
  
  ### confidence interval
  ci_sim[[seedNum]] <- cbind('2.5%' = theta_sim[,seedNum] + qnorm(0.025)* sqrt(var_sim[,seedNum]),
                                '97.5%' = theta_sim[,seedNum] + qnorm(0.975)* sqrt(var_sim[,seedNum]))
}

## Table 5 ####
result_tab <- cbind(
  ## relative Bias
  relBias_ft(q_mat = theta_micro, pop = theta_pop),
  relBias_ft(q_mat = theta_cart, pop = theta_pop),
  relBias_ft(q_mat = theta_sim, pop = theta_pop),
      
  ## relative RMSE
  relRMSE_ft(q_mat = theta_micro, pop = theta_pop),
  relRMSE_ft(q_mat = theta_cart, pop = theta_pop),
  relRMSE_ft(q_mat = theta_sim, pop = theta_pop),
  
  ## CI coverage
  rowMeans(sapply(ci_micro, function(ci) ciIndicate_ft(ci, pop=theta_pop))),
  rowMeans(sapply(ci_cart, function(ci) ciIndicate_ft(ci, pop=theta_pop))),
  rowMeans(sapply(ci_sim, function(ci) ciIndicate_ft(ci, pop=theta_pop)))
      )

write.csv(result_tab, file="03 Output\\Simulation tables\\simulation 1 results.csv")


# Simulation 2: Summary table ------------------------------------####
## Average missing rate ####
miss_rate_tab <- matrix(NA, nr=sample_num, nc=8) ## each variables
miss_rate_vec <- rep(NA, sample_num) ## by record

## each variables
for(seedNum in 1:sample_num) 
  miss_rate_tab[seedNum,] <- miss_index[,,seedNum] %>% colMeans

miss_rate_tab %>% colMeans %>% round(4)

for(seedNum in 1:sample_num){
  miss_rate_vec[seedNum] <- mean(rowSums(miss_index[,,seedNum])==0)
}
mean(miss_rate_vec)

## Synthetic Results ####

### synthetic: cc only (key - comp)####
theta_comp <- var_comp <- 
  matrix(NA, nc=sample_num, nr=8, dimnames = dimnames(c(x_var, y_var)))
ci_comp <- list()

for(seedNum in 1:sample_num){ ### each iteration... 
  ### prepare datasets
  data_comp <- syn_comp[[seedNum]]$synt_data %>% 
    lapply(function(s){
      dd <- within(s, {
        x1 <- ifelse(x1==1, 1, 0)
        x2 <- ifelse(x2==1, 1, 0)
        x3 <- ifelse(x3==1, 1, 0)
        x4 <- ifelse(x4==1, 1, 0)
        x5 <- ifelse(x5==1, 1, 0)
      })
      return(dd[,c(x_var, y_var)])
    } )
  
  
  ### pooled mean & rate
  theta_comp[,seedNum] <- data_comp %>%
    sapply(colMeans) %>% rowMeans
  
  ### pooled variance calculate
  Bm <- data_comp %>%
    sapply(colMeans) %>% apply(MARGIN=1, var)
  Vm <- data_comp %>%
    sapply(function(dd) apply(dd, MARGIN=2, function(vv) var(vv) * ( 1 - (nrow(dd) / pop_size) ) / nrow(dd) ) ) %>% rowMeans
  
  var_comp[,seedNum] <- (Bm / m_num) + Vm
  
  ### confidence interval
  ci_comp[[seedNum]] <- cbind('2.5%' = theta_comp[,seedNum] + qnorm(0.025)* sqrt(var_comp[,seedNum]),
                              '97.5%' = theta_comp[,seedNum] + qnorm(0.975)* sqrt(var_comp[,seedNum]))
}


### synthetic: Imputation (key - impu)####
theta_impu <- var_impu <- 
  matrix(NA, nc=sample_num, nr=8, dimnames = dimnames(c(x_var, y_var)))
ci_impu <- list()

for(seedNum in 1:sample_num){ ### each iteration... 
  ### prepare datasets
  data_impu <- syn_impu[[seedNum]]$synt_data %>% 
    lapply(function(s){
      dd <- within(s, {
        x1 <- ifelse(x1==1, 1, 0)
        x2 <- ifelse(x2==1, 1, 0)
        x3 <- ifelse(x3==1, 1, 0)
        x4 <- ifelse(x4==1, 1, 0)
        x5 <- ifelse(x5==1, 1, 0)
      })
      return(dd[,c(x_var, y_var)])
    } )
  
  
  ### pooled mean & rate
  theta_impu[,seedNum] <- data_impu %>%
    sapply(colMeans) %>% rowMeans
  
  ### pooled variance calculate
  Bm <- data_impu %>%
    sapply(colMeans) %>% apply(MARGIN=1, var)
  Vm <- data_impu %>%
    sapply(function(dd) apply(dd, MARGIN=2, function(vv) var(vv) * ( 1 - (nrow(dd) / pop_size) ) / nrow(dd) ) ) %>% rowMeans
  
  var_impu[,seedNum] <- (Bm / m_num) + Vm
  
  ### confidence interval
  ci_impu[[seedNum]] <- cbind('2.5%' = theta_impu[,seedNum] + qnorm(0.025)* sqrt(var_impu[,seedNum]),
                               '97.5%' = theta_impu[,seedNum] + qnorm(0.975)* sqrt(var_impu[,seedNum]))
}

## Table 7 ####
result_tab <- cbind(
  ## True
  theta_pop,
  
  ## E(q_hat)
  rowMeans(theta_comp),
  rowMeans(theta_impu),

  ## relative Bias
  relBias_ft(q_mat = theta_comp, pop = theta_pop),
  relBias_ft(q_mat = theta_impu, pop = theta_pop),

  ## relative RMSE
  relRMSE_ft(q_mat = theta_comp, pop = theta_pop),
  relRMSE_ft(q_mat = theta_impu, pop = theta_pop),
  
  ## CI coverage
  rowMeans(sapply(ci_comp, function(ci) ciIndicate_ft(ci, pop=theta_pop))),
  rowMeans(sapply(ci_impu, function(ci) ciIndicate_ft(ci, pop=theta_pop)))
)

write.csv(result_tab, file="03 Output\\Simulation tables\\simulation 2 results.csv")



# Simulation 2: Figure -------------------------------------------####
clevel <- seq(0.002, 0.03, 0.003)
myPalette <- sequential_hcl(length(clevel), "Heat")


seedNum <- 6; m <- 9

Origin_Y <- True_data[sample_index[,seedNum], y_var]
Synth_Y_1 <- syn_impu[[seedNum]]$synt_data[[m]][,y_var]
Synth_Y_2 <- syn_comp[[seedNum]]$synt_data[[m]][,y_var]

var_lim <- list(
  y0 = c(-9, 10),
  y1 = c(-12.5, 11),
  y2 = c(-12.5, 11)
)

## y0 ~ y1
png(filename="03 Output\\Contour plot\\contour_y1y0_orig.png", width=800, height=600)
par(cex.axis=2, cex.lab=2.2, mar=c(5,5,1,1)+0.1, family="serif", font=2)
z <- kde2d(Origin_Y$y1, Origin_Y$y0, n=100, lims = c(var_lim$y1, var_lim$y0))
plot(y0~y1, data=Origin_Y, xlim=var_lim$y1, ylim=var_lim$y0, pch=20, col="grey70", ylab=expression(Y[0]), xlab=expression(Y[1]))
contour(z, lwd=2, add=TRUE, col=myPalette, levels=clevel, labcex=1.5)
dev.off()

png(filename="03 Output\\Contour plot\\contour_y1y0_impute.png", width=800, height=600)
par(cex.axis=2, cex.lab=2.2, mar=c(5,5,1,1)+0.1, family="serif", font=2)
z <- kde2d(Synth_Y_1$y1, Synth_Y_1$y0, n=100, lims = c(var_lim$y1, var_lim$y0))
plot(y0~y1, data=Synth_Y_1, xlim=var_lim$y1, ylim=var_lim$y0, pch=20, col="grey70", ylab=expression(Y[0]), xlab=expression(Y[1]))
contour(z, lwd=2, add=TRUE, col=myPalette, levels=clevel, labcex=1.5)
dev.off()

png(filename="03 Output\\Contour plot\\contour_y1y0_cconly.png", width=800, height=600)
par(cex.axis=2, cex.lab=2.2, mar=c(5,5,1,1)+0.1, family="serif", font=2)
z <- kde2d(Synth_Y_2$y1, Synth_Y_2$y0, n=100, lims = c(var_lim$y1, var_lim$y0))
plot(y0~y1, data=Synth_Y_2, xlim=var_lim$y1, ylim=var_lim$y0, pch=20, col="grey70", ylab=expression(Y[0]), xlab=expression(Y[1]))
contour(z, lwd=2, add=TRUE, col=myPalette, levels=clevel, labcex=1.5)
dev.off()

#### y0 ~ y2
png(filename="03 Output\\Contour plot\\contour_y2y0_orig.png", width=800, height=600)
par(cex.axis=2, cex.lab=2.2, mar=c(5,5,1,1)+0.1, family="serif", font=2)
z <- kde2d(Origin_Y$y2, Origin_Y$y0, n=100, lims = c(var_lim$y2, var_lim$y0))
plot(y0~y2, data=Origin_Y, xlim=var_lim$y2, ylim=var_lim$y0, pch=20, col="grey70", ylab=expression(Y[0]), xlab=expression(Y[2]))
contour(z, lwd=2, add=TRUE, col=myPalette, levels=clevel, labcex=1.5)
dev.off()

png(filename="03 Output\\Contour plot\\contour_y2y0_impute.png", width=800, height=600)
par(cex.axis=2, cex.lab=2.2, mar=c(5,5,1,1)+0.1, family="serif", font=2)
z <- kde2d(Synth_Y_1$y2, Synth_Y_1$y0, n=100, lims = c(var_lim$y2, var_lim$y0))
plot(y0~y2, data=Synth_Y_1, xlim=var_lim$y2, ylim=var_lim$y0, pch=20, col="grey70", ylab=expression(Y[0]), xlab=expression(Y[2]))
contour(z, lwd=2, add=TRUE, col=myPalette, levels=clevel, labcex=1.5)
dev.off()

png(filename="03 Output\\Contour plot\\contour_y2y0_cconly.png", width=800, height=600)
par(cex.axis=2, cex.lab=2.2, mar=c(5,5,1,1)+0.1, family="serif", font=2)
z <- kde2d(Synth_Y_2$y2, Synth_Y_2$y0, n=100, lims = c(var_lim$y2, var_lim$y0))
plot(y0~y2, data=Synth_Y_2, xlim=var_lim$y2, ylim=var_lim$y0, pch=20, col="grey70", ylab=expression(Y[0]), xlab=expression(Y[2]))
contour(z, lwd=2, add=TRUE, col=myPalette, levels=clevel, labcex=1.5)
dev.off()


