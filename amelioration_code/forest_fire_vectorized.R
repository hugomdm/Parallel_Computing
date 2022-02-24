
## File: forest_file_vectorized.r
## Students : Lia Furtado and Hugo Vinsion
## Projet Parallel Computing 2021 - Code improvements
# forest fire simulation - Capther 21 (21.2.3 Forest fire model)

## Description : Improved code with a vectorized neighborhood solution

## Date : 21 February 2022

find_infected_neighbors_vectorized <- function(infection_matrix) {
  #' Function to calculate for each point in the infection_matrix the neighbors that were infected 
  #' from all the 8 edges
  #' For point (x, y) the points (x + 1, y + 1), (x + 1, y), (x + 1, y + 1),
  #'(x, y  + 1), (x, y + 1), (x + 1, y + 1), (x + 1, y), and (x + 1, y + 1) are considered neighbors.
  #'
  #' @param infection_matrix : main infection_matrix that maps an individual and 
  #' its current state state (unburnt, on fire or burnt out)  
  #'
  #' @return number_infected_neigh: vector with the number of infected individuals for each point in a vector
  #' 
  #setting a the main matrix with padding 
  infection_matrix <- t(infection_matrix)
  n <- nrow(infection_matrix)
  mat.pad = rbind(NA, cbind(NA, infection_matrix, NA), NA)
  
  ind = 2:(n + 1) # row/column indices of the "middle"
  
  #calculating all the 8 edges and the point neighborhoods
  neigh = rbind(N  = c(mat.pad[ind - 1, ind    ]),
                NE = c(mat.pad[ind - 1, ind + 1]),
                E  = c(mat.pad[ind    , ind + 1]),
                SE = c(mat.pad[ind + 1, ind + 1]),
                S  = c(mat.pad[ind + 1, ind    ]),
                SW = c(mat.pad[ind + 1, ind - 1]),
                W  = c(mat.pad[ind    , ind - 1]),
                NW = c(mat.pad[ind - 1, ind - 1]), 
                R = c(mat.pad[ind , ind ]))
  #searching in the neighbors the number of points that are infected ( equal to 1)
  number_infected_neigh <- colSums(neigh==1, na.rm=TRUE)
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
forest_fire_vectorized <- function(infection_matrix, alpha, beta, pausing = FALSE) {
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
  plot(c(1,nrow(infection_matrix)), c(1,ncol(infection_matrix)), type = "n", xlab = "", ylab = "")
  # main loop
  burning <- TRUE
  while (burning) {
    burning <- FALSE
    # check if pausing between updates
    if (pausing) {
      #Hit any key and plot the fire simulation
      input <- readline("hit any key to continue")
      forest_fire_plot(infection_matrix)
    }
    #get the vector with the number of infected neighbors for each value in the matrix
    number_infected <- find_infected_neighbors_vectorized(infection_matrix)
    #turning the infection matrix into a vector
    infection_vector =c(t(infection_matrix))
    # make a copy of the infection_matrix
    infection_vector_copy <- infection_vector
    #iterate though the infection vector 
    for (i in 1:length(infection_vector)) {
        #if an individual is unburt
        if (infection_vector[i] == 2) {
          #check number of neighbors are infected 
          #if the probability is bigger than the probability remaining uninfected (1 - alpha)^n_infected
          if (runif(1) > (1 - alpha)^number_infected[i]) {
            #it changes status for on fire
            infection_vector_copy[i] <- 1
          }
          #else if an individual is on fire
        } else if (infection_vector[i] == 1) {
          #the forest will continue burning 
          burning <- TRUE
          #if probability if less than beta an individual is removed
          if (runif(1) < beta) {
            #it will be burn out
            infection_vector_copy[i] <- 0
          }
        }
    }
    infection_matrix <- matrix(unlist(infection_vector_copy), ncol = ncol(infection_matrix), byrow = TRUE)
  }
  return(infection_matrix)
}
#---------------------- Code to Test the solution --------------------------- #

# set.seed(3)
# infection_matrix <- matrix(2, 21, 21)
# infection_matrix[11, 11] <- 1
# # big fires
# result <- forest_fire_vectorized(infection_matrix, .2, .4, FALSE)
# # # # Tests to be run when modifying the code
# runTest <- function(result) {
#   stopifnot(result[1, 1] ==  2)
#   stopifnot(result[7,12] == 2)
#   stopifnot(result[11,19] == 0)
#   stopifnot(result[15,8] == 2)
#   stopifnot(result[15,7] == 0)
#   stopifnot(result[21,17] == 0)
# }
# runTest(result)

