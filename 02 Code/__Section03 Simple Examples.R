#---------------------------------------------------------------------#
# 
# Simple Example
# 
#---------------------------------------------------------------------#

# Setting --------------------------------------------------------####
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)

if(!requireNamespace("synMicrodata")) install.packages("synMicrodata")
if(!requireNamespace("dplyr")) install.packages("dplyr")

library(synMicrodata)
library(dplyr)


rm(list=ls())
# folderName <- "C:\\Users\\gmle3\\OneDriv\\Research\\04 Synthetic using NB\\"



# Generate Synthetic Datasets ------------------------------------####
## preparing to generate synthetic datasets ####
dat_obj <- readData(Y_input = iris[,1:4],
                    X_input = data.frame(Species=iris[,5]))
str(dat_obj)

mod_obj <- createModel(data_obj=dat_obj, max_R_S_K = c(10, 30, 20))


## generating synthetic datasets ####
syn_results <- multipleSyn(dat_obj, mod_obj, n_burnin = 100, m = 6, 
                           interval_btw_Syn = 50, show_iter = FALSE)

print(syn_results)
names(syn_results)


# Check Synthetic Results ----------------------------------------####
## synthetic datasets ####
str(syn_results$synt_data)

head(syn_results$synt_data[[3]])

## Mixture Components ####
str(syn_results$comp_mat)

apply(syn_results$comp_mat$r_mat, MARGIN=1, function(r) length(unique(r)))
apply(syn_results$comp_mat$s_mat, MARGIN=1, function(s) length(unique(s)))
apply(syn_results$comp_mat$k_mat, MARGIN=1, function(k) length(unique(k)))


## plot ####
names(syn_results$orig_data)
par(mfrow=c(2,3), cex=1.5, mar = c(3,3,3,1))

### 1 continuous variable
plot(syn_results, vars="Sepal.Length")
# plot(syn_results, vars=1) ## same results

### 2 or more continuous variables
plot(syn_results, vars=c("Sepal.Length", "Sepal.Width"))

## categorical variables
plot(syn_results, vars=c("Species"))
