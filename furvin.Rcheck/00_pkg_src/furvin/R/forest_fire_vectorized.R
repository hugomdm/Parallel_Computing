
## File: forest_file_vectorized.r
## Students : Lia Furtado and Hugo Vinsion
## Projet Parallel Computing 2021 - Code improvements
# forest fire simulation - Capther 21 (21.2.3 Forest fire model)

## Description : Improved code with a vectorized neighborhood solution

## Date : 21 February 2022

#-------------- Functions ----------------------

#' @title forest_file_vectorized.r: Simulate Forest Fire using vectorized techniques 
#' @description  Function to simulate a forest fire epidemic model
#' For infection_matrix[i, j] = 2, person is unburnt; 1 for on fire; 0 for burnt out.
#' An on fire individual can only infect unburnt individuals if they are neighbors.
#' @param infection_matrix : main infection_matrix that maps an individual and 
#' its current state state (unburnt, on fire or burnt out)  
#' @param alpha : probability of an infected individual of infecting each of its susceptible neighbors
#' @param beta : probability of an individual that was on fire and had a chance of 
#' affecting people of getting unburnt 
#' @param pausing : boolean value to print the plot of the forest or not
#'
#' @return infection_matrix: returns the updated infection_matrix after the simulation
#' @export
#'
#' @examples set.seed(3)
#'infection_matrix <- matrix(2, 21, 21)
#'infection_matrix[11, 11] <- 1
#' result = forest_fire_vectorized(infection_matrix, .2, .4, FALSE)
#' 
forest_fire_vectorized <- function(infection_matrix, alpha, beta, pausing = FALSE) {
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

