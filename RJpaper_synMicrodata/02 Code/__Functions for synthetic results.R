#---------------------------------------------------------------------#
# 
# Functions for summarizing synthetic results
# 
#---------------------------------------------------------------------#

## Repeated Simulation Results ####
### ci: data.frame of CI (row: variables, col: 2.5%, 97.5%)
### q_mat: matrix of q_hat (row: variables)
### pop: vector of population parameters

ciIndicate_ft <- function(ci, pop){
  return( (ci[,1] < pop) & (pop < ci[,2]) )
}
relBias_ft <- function(q_mat, pop){
  return( ( rowMeans(q_mat) - pop ) / abs( pop ) )
}
relRMSE_ft <- function(q_mat, pop){
  return(sqrt(rowMeans((q_mat - pop)^2)) / abs(pop))
}


