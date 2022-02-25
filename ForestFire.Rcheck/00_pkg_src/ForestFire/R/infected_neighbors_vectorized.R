#' Function to calculate for each point in the infection_matrix the neighbors that were infected 
#' from all the 8 edges
#' For point (x, y) the points (x + 1, y + 1), (x + 1, y), (x + 1, y + 1),
#'(x, y  + 1), (x, y + 1), (x + 1, y + 1), (x + 1, y), and (x + 1, y + 1) are considered neighbors.
#'
#' @param infection_matrix : main infection_matrix that maps an individual and 
#' its current state state (unburnt, on fire or burnt out)  
#'
#' @return number_infected_neigh: vector with 
#' the number of infected individuals for each point in a vector
#' @export
#'
#' @examples
#' 
find_infected_neighbors_vectorized <- function(infection_matrix) {
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