## File: forest_file_optimized.r
## Students : Lia Furtado and Hugo Vinsion
## Description : Projet Parallel Computing 2021 - Code improvements
# forest fire simulation - Capther 21 (21.2.3 Forest fire model)

## Date : 21 February 2022

find_infected_neighbors_apply <- function(infection_matrix) {
  #' Function to find for the point infection_matrix[i,j] the neighbors that were infected 
  #' from all the 8 edges from a point in the matrix
  #' For point (x, y) the points (x + 1, y + 1), (x + 1, y), (x + 1, y + 1),
  #'(x, y  + 1), (x, y + 1), (x + 1, y + 1), (x + 1, y), and (x + 1, y + 1) are considered neighbors.
  #'
  #' @param infection_matrix : main infection_matrix that maps an individual and 
  #' its current state state (unburnt, on fire or burnt out)  
  #' @param i: current row in the  infection_matrix to detect neighbors
  #' @param j: current column in the  infection_matrix to detect neighbors
  #'
  #' @return number_infected: number of infected individuals after checking the 8 neighbors of a point
  #' 
  #'  
  #'  
  infection_matrix <- t(infection_matrix)
  n <- nrow(infection_matrix)
  mat.pad = rbind(NA, cbind(NA, infection_matrix, NA), NA)
  
  ind = 2:(n + 1) # row/column indices of the "middle"
  
  neigh = rbind(N  = c(mat.pad[ind - 1, ind    ]),
                NE = c(mat.pad[ind - 1, ind + 1]),
                E  = c(mat.pad[ind    , ind + 1]),
                SE = c(mat.pad[ind + 1, ind + 1]),
                S  = c(mat.pad[ind + 1, ind    ]),
                SW = c(mat.pad[ind + 1, ind - 1]),
                W  = c(mat.pad[ind    , ind - 1]),
                NW = c(mat.pad[ind - 1, ind - 1]), 
                R = c(mat.pad[ind , ind ]))
  
  
  number_infected_neigh <- apply(neigh, 2, function(x) {
    sum(x == 1, na.rm=T)})
  return(number_infected_neigh)
}
forest_fire_plot <- function(infection_matrix) {
  # plot infected and removed individuals
  for (i in 1:nrow(infection_matrix)) {
    for (j in 1:ncol(infection_matrix)) {
      if (infection_matrix[i,j] == 1) {
        points(i, j, col = "red", pch = 19)
      }
      else if (infection_matrix[i,j] == 0) {
        points(i, j, col = "grey", pch = 19)
      }
    }
  }
}
forest_fire_apply <- function(infection_matrix, alpha, beta, pausing = FALSE) {
  #' Function to simulate a forest fire epidemic model
  #' For infection_matrix[i, j] = 2, person is unburnt; 1 for on fire; 0 for burnt out.
  #' An on fire individual can only infect unburnt individuals if they are neighbors.
  #' @param infection_matrix : main infection_matrix that maps an individual and 
  #' its current state state (unburnt, on fire or burnt out)  
  #' @param alpha: probability of an infected individual of infecting each of its susceptible neighbors
  #' @param beta: probability of an individual that was on fire and had a chance of 
  #' affecting people of getting unburnt 
  #' @param pausing 
  #'
  #' @return infection_matrix: returns the updated infection_matrix after the simulation
  #'
  #plot(c(1,nrow(infection_matrix)), c(1,ncol(infection_matrix)), type = "n", xlab = "", ylab = "")
  #forest_fire_plot(infection_matrix)
  # main loop
  
  for (iterator in 1:20){
    # check if pausing between updates
    if (pausing) {
      input <- readline("hit any key to continue")
    }
    # make a copy of the infection_matrix
    n_infected <- find_infected_neighbors_apply(infection_matrix)
    n_infected <- matrix(unlist(n_infected), nrow(infection_matrix))
    infection_matrix_copy <- infection_matrix
    #iterate though the infection_matrix 
    for (i in 1:nrow(infection_matrix)) {
      for (j in 1:ncol(infection_matrix)) {
        #if an individual is unburt
        if (infection_matrix[i, j] == 2) {
          #check number of neighbors are infected 
          #if the probability is bigger than the probability remaining uninfected (1 alpha)^n_infected
          if (runif(1) > (1 - alpha)^n_infected[j,i]) {
            #it changes status for on fire
            infection_matrix_copy[i, j] <- 1
          }
          #else if an individual is on fire
        } else if (infection_matrix[i, j] == 1) {
          #the forest will continue burning 
          #if probability if less than beta an individual is removed
          if (runif(1) < beta) {
            #it will be burn out
            infection_matrix_copy[i, j] <- 0
          }
        }
        
        
      }
    }
    
    infection_matrix <- infection_matrix_copy

    # plot
    #forest_fire_plot(infection_matrix)
  }
  return(infection_matrix)
}

# set.seed(3)
# # # #initialize a infection_matrix of 21x21 with only 2 values (susceptible people)
# infection_matrix <- matrix(2, 21, 21)
# # # #in the central position of the infection_matrix set a 1 value
# infection_matrix[11, 11] <- 1
# 
# result <- forest_fire_apply(infection_matrix, .2, .4, FALSE)
# # # Tests to be run when modifying the code
# runTest <- function(result) {
#   stopifnot(result[1, 5] ==  1)
#   stopifnot(result[7,12] == 2)
#   stopifnot(result[11,19] == 0)
#   stopifnot(result[21,11] == 0)
#   stopifnot(result[15,7] == 1)
#   stopifnot(result[21,17] == 2)
# }
# runTest(result)
