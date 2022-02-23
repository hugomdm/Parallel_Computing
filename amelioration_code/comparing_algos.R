

# ---------------  TO BUILD -------
#Set the current directory as your working directory in R with the 'setwd' command

library(Rcpp)
library(microbenchmark)


source("forest_fire_original.R")
source("forest_file_commented.R")
source("forest_file_neighboord_optimized.R")
sourceCpp("forest_fire_rcpp.cpp")


#sourceCpp("do3_optimization_rcpp.cpp")



#----------- Code to Compare the algorithms ----------------------------
# 1 algo: Initial One with no Optimization 
# 2 algo: Cleaned and More memory efficient code
#3 algo: Made in the C++ language with the help of RCPP for R
#4 algo: Made in Python code

set.seed(3)
infection_matrix <- matrix(2, 21, 21)
infection_matrix[11, 11] <- 1
#forest_fire_commented(infection_matrix, .2, .4, FALSE)
microbenchmark(#forest.fire.original(infection_matrix, .2, .4, FALSE), 
               forest_fire_commented(infection_matrix, .2, .4, FALSE),
               forest_fire_opt_neigh_commented(infection_matrix, .2, .4, FALSE),
               forest_fire_rcpp(infection_matrix, .2, .4, FALSE),
               times=10)

#system.time(main_cpp(c(14),10,5,0))


#Evaluate time for different k values using system.time
# count <- 1 
# list_k = c(1,5, 10, 12, 14,15) # add 10,20 in list_k
# calcul_time <- rep(NA, length(list_k))
# for (k in list_k){
#   print(k)
#   res <- system.time(main_cpp(c(k),10,5,0))
#   calcul_time[[count]] <- res[3]
#   count <- count + 1
# } 
# 
# plot(list_k, calcul_time,  type = "b", pch = 19, col = "red", xlab = "k", ylab = "temps de calcul (s)")

