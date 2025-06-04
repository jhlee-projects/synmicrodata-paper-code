#---------------------------------------------------------------------#
# 
# Generate Simulation Datasets
# 
#---------------------------------------------------------------------#

## Setting --------------------------------------------------------####
### install packages...
if(!requireNamespace('mvtnorm')){
  install.packages("mvtnorm")
}
if(!requireNamespace('fastDummies')){
  install.packages("fastDummies")
}

library(MASS) # for "mvrnorm"
library(mvtnorm) # for "dmvnorm"

### dir check...
setwd("~Directory") ## working directory
if(!dir.exists("01 Data")) dir.create("01 Data")

rm(list=ls()); gc()


## Population -----------------------------------------------------####
pop_size <- 1e+06 ## population size

set.seed(1111)

### Components ####
## top level component
prob_k <- c(0.05, 0.40, 0.30, 0.15, 0.10)

k <- sample(5, size=pop_size, replace=TRUE, prob=prob_k)

r <- s <- numeric(pop_size)

## categorical components 
s[k==1] <- sample(5, size=sum(k==1), replace=TRUE, prob=c(0.15, 0.10, 0.35, 0.15, 0.25))
s[k==2] <- sample(5, size=sum(k==2), replace=TRUE, prob=c(0.35, 0.20, 0.15, 0.15, 0.15))
s[k==3] <- sample(5, size=sum(k==3), replace=TRUE, prob=c(0.25, 0.15, 0.15, 0.35, 0.10))
s[k==4] <- sample(5, size=sum(k==4), replace=TRUE, prob=c(0.15, 0.25, 0.15, 0.15, 0.30))
s[k==5] <- sample(5, size=sum(k==5), replace=TRUE, prob=c(0.45, 0.15, 0.15, 0.15, 0.10))

## continuous components
r[k==1] <- sample(3, size=sum(k==1), replace=TRUE, prob=c(0.50, 0.20, 0.30))
r[k==2] <- sample(3, size=sum(k==2), replace=TRUE, prob=c(0.70, 0.15, 0.15))
r[k==3] <- sample(3, size=sum(k==3), replace=TRUE, prob=c(0.40, 0.20, 0.40))
r[k==4] <- sample(3, size=sum(k==4), replace=TRUE, prob=c(0.20, 0.50, 0.30))
r[k==5] <- sample(3, size=sum(k==5), replace=TRUE, prob=c(0.30, 0.20, 0.50))


### Categorical variables ####
x1 <- x2 <- x3 <- x4 <- x5 <- numeric(pop_size)

## given s==1
x1[s==1] <- sample(2, size=sum(s==1), replace=TRUE, prob=c(0.60, 0.40))
x2[s==1] <- sample(4, size=sum(s==1), replace=TRUE, prob=c(0.55, 0.15, 0.20, 0.10))
x3[s==1] <- sample(3, size=sum(s==1), replace=TRUE, prob=c(0.20, 0.10, 0.70))
x4[s==1] <- sample(2, size=sum(s==1), replace=TRUE, prob=c(0.50, 0.50))
x5[s==1] <- sample(3, size=sum(s==1), replace=TRUE, prob=c(0.30, 0.50, 0.20))

## given s==2
x1[s==2] <- sample(2, size=sum(s==2), replace=TRUE, prob=c(0.80, 0.20))
x2[s==2] <- sample(4, size=sum(s==2), replace=TRUE, prob=c(0.55, 0.35, 0.15, 0.05))
x3[s==2] <- sample(3, size=sum(s==2), replace=TRUE, prob=c(0.20, 0.20, 0.60))
x4[s==2] <- sample(2, size=sum(s==2), replace=TRUE, prob=c(0.90, 0.10))
x5[s==2] <- sample(3, size=sum(s==2), replace=TRUE, prob=c(0.50, 0.10, 0.40))

## given s==3
x1[s==3] <- sample(2, size=sum(s==3), replace=TRUE, prob=c(0.55, 0.45))
x2[s==3] <- sample(4, size=sum(s==3), replace=TRUE, prob=c(0.25, 0.15, 0.05, 0.55))
x3[s==3] <- sample(3, size=sum(s==3), replace=TRUE, prob=c(0.20, 0.30, 0.50))
x4[s==3] <- sample(2, size=sum(s==3), replace=TRUE, prob=c(0.50, 0.50))
x5[s==3] <- sample(3, size=sum(s==3), replace=TRUE, prob=c(0.50, 0.30, 0.20))

## given s==4
x1[s==4] <- sample(2, size=sum(s==4), replace=TRUE, prob=c(0.55, 0.45))
x2[s==4] <- sample(4, size=sum(s==4), replace=TRUE, prob=c(0.25, 0.35, 0.05, 0.35))
x3[s==4] <- sample(3, size=sum(s==4), replace=TRUE, prob=c(0.30, 0.40, 0.30))
x4[s==4] <- sample(2, size=sum(s==4), replace=TRUE, prob=c(0.65, 0.35))
x5[s==4] <- sample(3, size=sum(s==4), replace=TRUE, prob=c(0.50, 0.20, 0.30))

## given s==5
x1[s==5] <- sample(2, size=sum(s==5), replace=TRUE, prob=c(0.25, 0.75))
x2[s==5] <- sample(4, size=sum(s==5), replace=TRUE, prob=c(0.45, 0.05, 0.15, 0.35))
x3[s==5] <- sample(3, size=sum(s==5), replace=TRUE, prob=c(0.45, 0.20, 0.35))
x4[s==5] <- sample(2, size=sum(s==5), replace=TRUE, prob=c(0.75, 0.25))
x5[s==5] <- sample(3, size=sum(s==5), replace=TRUE, prob=c(0.75, 0.15, 0.10))


### Continuous variables ####
## design matrix
x_mat <- cbind(x1, x2, x3, x4, x5)
x_mat_dummies <- fastDummies::dummy_cols(x_mat, select_columns = paste0("x", 1:5))
x_mat_dummies <- x_mat_dummies[,paste0("x", c(1, rep(2,3), rep(3,2), 4, rep(5,2)), "_", c(2, 2:4, 2:3, 2, 2:3))]
x_mat_dummies <- as.matrix(cbind(1, x_mat_dummies))

## parameter setting
beta_y0_1 <- c( 0.8,  0.5,  1.2,  2.5,  0.7,  1.3,  0.4,  0.8,  1.2,  0.5)
beta_y1_1 <- c( 1.5,  0.5,  1.0,  2.0,  1.5,  2.0,  0.5,  0.5,  0.3,  2.5)
beta_y2_1 <- c(-0.3, -1.5, -0.7, -2.5, -0.5, -2.0, -2.5,  2.5,  1.3,  1.5)

beta_y0_2 <- c( 0.3,  1.5, -1.2, -1.5,  0.7,  1.3,  1.5,  2.8, -1.2,  1.5)
beta_y1_2 <- c(-2.5, -0.1, -3.1, -2.2,  1.5, -2.0, -0.5, -2.5, -1.3, -0.5)
beta_y2_2 <- c( 1.3,  0.5, -0.7,  1.5,  0.5, -1.5,  1.5,  0.5, -1.3,  0.3)

beta_y0_3 <- rep(-1, 10)
beta_y1_3 <- rep(1, 10)
beta_y2_3 <- rep(-1, 10)

mu_1 <- cbind(x_mat_dummies%*%beta_y0_1, x_mat_dummies%*%beta_y1_1, x_mat_dummies%*%beta_y2_1)
mu_2 <- cbind(x_mat_dummies%*%beta_y0_2, x_mat_dummies%*%beta_y1_2, x_mat_dummies%*%beta_y2_2)
mu_3 <- cbind(x_mat_dummies%*%beta_y0_3, x_mat_dummies%*%beta_y1_3, x_mat_dummies%*%beta_y2_3)


Sigma_1 <- matrix(c( 1.0,  0.3,  0.5,
                     0.3,  1.0,  0.5,
                     0.5,  0.5,  1.0), nr=3, nc=3, byrow=TRUE)

Sigma_2 <- matrix(c( 1.0,  0.2, -0.5,
                     0.2,  1.0, -0.6,
                     -0.5, -0.6,  1.0), nr=3, nc=3, byrow=TRUE)

Sigma_3 <- matrix(c( 1.0,  0.5,  0.1,
                     0.5,  1.0,  0.0,
                     0.1,  0.0,  1.0), nr=3, nc=3, byrow=TRUE)

## genearte continuous variables
y_mat <- matrix(NA, nr=pop_size, nc=3)

for(i in 1:pop_size){
  switch(r[i],
         '1'={ y_mat[i,] <- mvrnorm(n = 1, mu= mu_1[i,], Sigma = Sigma_1)  },
         '2'={ y_mat[i,] <- mvrnorm(n = 1, mu= mu_2[i,], Sigma = Sigma_1)  },
         '3'={ y_mat[i,] <- mvrnorm(n = 1, mu= mu_3[i,], Sigma = Sigma_3)  })
}

colnames(y_mat) <- paste0("y", 0:2)

### Data format ####
True_data <- cbind(x_mat, y_mat) ## combine categorical & continuous

True_data <- within(data.frame(True_data), { ## categorical variables -> 'factor' class
  x1 <- factor(x1, levels=1:2)
  x2 <- factor(x2, levels=1:4)
  x3 <- factor(x3, levels=1:3)
  x4 <- factor(x4, levels=1:2)
  x5 <- factor(x5, levels=1:3)
})

True_component <- list(True_k_i_vec = k, True_s_i_vec = s, True_r_i_vec = r) ## component

save(True_data, True_component,
     file="01 Data\\simulation dataset (population).rda")


## Sample Index : for simulation 1 & 2 ----------------------------####
rm(list=ls()); gc() ## clear environment

load("01 Data\\simulation dataset (population).rda") ## load simulation population

sample_num <- 500 ## number of iteration
sample_size <- 1000 ## sample size

### sample index
sample_index <- matrix(NA, nrow=sample_size, ncol=sample_num)

for(seedNum in 1:sample_num){ ## Draw 500 samples with a sample size of 1000
  set.seed(seedNum + 100) ## seed number
  sample_index[,seedNum] <- sample(nrow(True_data), size=sample_size, replace=FALSE)
}

save(sample_num, sample_size, sample_index, 
     file=paste0("01 Data\\sample index (size ", sample_size, ").rda"))

## MAR Generation : for simulation 2 -------------------------------####
expit_fn = function(x) { 1/(1+exp(-x)) } # inverse of the logit function

### missing generation ####
miss_index <- replicate(sample_num, matrix(0, nrow=sample_size, ncol=ncol(True_data)))

for(seedNum in 1:sample_num){
  set.seed(seedNum + 10000)
  miss_index[,1,seedNum] <- rbinom(sample_size, 1, expit_fn(-2.2 + (True_data[sample_index[,seedNum],2]==1)*0.1 + (True_data[sample_index[,seedNum],2]==2)*2.5) )
  miss_index[,2,seedNum] <- rbinom(sample_size, 1, expit_fn(-3.0 + (True_data[sample_index[,seedNum],3]==1)*3.0 + (True_data[sample_index[,seedNum],3]==2)*0.2) )
  miss_index[,4,seedNum] <- rbinom(sample_size, 1, expit_fn(-1.0 + (True_data[sample_index[,seedNum],5]==1)*0.8) )
  miss_index[,5,seedNum] <- rbinom(sample_size, 1, expit_fn(-2.7 + True_data[sample_index[,seedNum],6]*0.7) )
  
  miss_index[,6,seedNum] <- rbinom(sample_size, 1, expit_fn(-3.8 + True_data[sample_index[,seedNum],7]*0.4 + True_data[sample_index[,seedNum],8]*0.1 ) )
}

save(miss_index, file=paste0("01 Data\\miss index (size ", sample_size, ").rda"))



