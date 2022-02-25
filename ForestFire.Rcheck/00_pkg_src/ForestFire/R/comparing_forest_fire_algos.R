
## File: comparing_forest_fire_algos.r
## Students : Lia Furtado and Hugo Vinsion
## Description : Projet Parallel Computing 2021 - Code improvements
# forest fire simulation - Chapter 21 (21.2.3 Forest fire model)

## Description : Code to compare the time of each solution 

## Date : 21 February 2022

# ---------------  TO BUILD -------
#Set the current directory as your working directory in R with the 'setwd' command

#library(Rcpp)
#library(microbenchmark)

#source("forest_fire_original.R")
#source("forest_file_commented.R")
#source("forest_fire_vectorized.R")
#source("forest_fire_apply.R")
#sourceCpp("forest_fire_rcpp.cpp")

#----------- Code to Compare the algorithms ----------------------------
# 1 algo: Initial code from Chapter 21 (Forest Fire Simulation)
# 2 algo: Cleaned, commented and More memory efficient code
# 3 algo: Optimized the neighboord strategy with apply
# 4 algo: Optimized the neighboord strategy with vectorized functions
# 5 algo: Made in the C++ language with the help of RCPP for R

#' Title
#'Code to compare the time of each solution 
#' @return
#' result: microbenchmark of the algos comparision
#' @export 
#'
#' @examples
#' 
comparing_algos <- function(){
set.seed(3)
infection_matrix <- matrix(2, 21, 21)
infection_matrix[11, 11] <- 1

result <- microbenchmark(
               forest_fire_commented(infection_matrix, .2, .4, FALSE),
               forest_fire_apply(infection_matrix, .2, .4, FALSE),
               forest_fire_vectorized(infection_matrix, .2, .4, FALSE),
               forest_fire_rcpp(infection_matrix, .2, .4, FALSE),
               times=20)
return(result)
}
#The best performances was the vectorized implementation and the rcpp one, 
#that manage to reduce significantly the time of execution. 
#comparing_algos()
