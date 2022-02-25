#' Function to find for the point infection_matrix[i,j] the neighbors that were infected 
#' from all the 8 edges from a point in the matrix
#' For point (x, y) the points (x + 1, y + 1), (x + 1, y), (x + 1, y + 1),
#'(x, y  + 1), (x, y + 1), (x + 1, y + 1), (x + 1, y), and (x + 1, y + 1) are considered neighbors.
#' @param infection_matrix : main infection_matrix that maps an individual and 
#' its current state state (unburnt, on fire or burnt out)  
#' @param i : current row in the  infection_matrix to detect neighbors
#' @param j : current column in the  infection_matrix to detect neighbors
#' 
#' @return number_infected: number of infected individuals after checking the 8 neighbors of a point
#' @export
#'
#' @examples
#'
#' 
find_infected_neighbors <- function(infection_matrix, i, j) {
  number_infected <- 0
  # sum across row i - 1
  if (i > 1) {
    if (j > 1) {
      number_infected <- number_infected + (infection_matrix[i-1, j-1] == 1)
    }
    number_infected <- number_infected + (infection_matrix[i-1, j] == 1)
    if (j < ncol(infection_matrix)) {
      number_infected <- number_infected + (infection_matrix[i-1, j+1] == 1)
    }
  }
  # sum across row i
  if (j > 1) {
    number_infected <- number_infected + (infection_matrix[i, j-1] == 1)
  }
  number_infected <- number_infected + (infection_matrix[i, j] == 1)
  if (j < ncol(infection_matrix)) {
    number_infected <- number_infected + (infection_matrix[i, j+1] == 1)
  }
  # sum across row i + 1
  if (i < nrow(infection_matrix)) {
    if (j > 1) {
      number_infected <- number_infected + (infection_matrix[i+1, j-1] == 1)
    }
    number_infected <- number_infected + (infection_matrix[i+1, j] == 1)
    if (j < ncol(infection_matrix)) {
      number_infected <- number_infected + (infection_matrix[i+1, j+1] == 1)
    }
  }
  return(number_infected)
}