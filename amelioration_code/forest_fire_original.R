# program: spuRs/resources/scripts/forest_fire.r
# forest fire simulation
rm(list = ls())

#' Title
#'
#' @param A 
#' @param i 
#' @param j 
#'
#' @return
#' @export
#'
#' @examples
neighbours <- function(A, i, j) {
  # calculate number of neighbours of A[i,j] that are infected
  # we have to check for the edge of the grid
  nbrs <- 0
  # sum across row i - 1
  if (i > 1) {
    if (j > 1) nbrs <- nbrs + (A[i-1, j-1] == 1)
    nbrs <- nbrs + (A[i-1, j] == 1)
    if (j < ncol(A)) nbrs <- nbrs + (A[i-1, j+1] == 1)
  }
  # sum across row i
  if (j > 1) nbrs <- nbrs + (A[i, j-1] == 1)
  nbrs <- nbrs + (A[i, j] == 1)
  if (j < ncol(A)) nbrs <- nbrs + (A[i, j+1] == 1)
  # sum across row i + 1
  if (i < nrow(A)) {
    if (j > 1) nbrs <- nbrs + (A[i+1, j-1] == 1)
    nbrs <- nbrs + (A[i+1, j] == 1)
    if (j < ncol(A)) nbrs <- nbrs + (A[i+1, j+1] == 1)
  }
  return(nbrs)
}
#' Function plot the spread of fire representation with points 
#'
#' @param infection_matrix main infection_matrix that maps an individual and 
#' its current state state (unburnt, on fire or burnt out)  
#'
#' @return
#' @export
#'
#' @examples
#' forest_fire_plot(infection_matrix)
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
#' Initial Code of the Forest.fire simulation
#'
#' @param X 
#' @param a 
#' @param b 
#' @param pausing 
#'
#' @return
#' @export
#'
#' @examples
forest.fire <- function(X, a, b, pausing = FALSE) {
  # simulate forest fire epidemic model
  # X[i, j] = 2 for susceptible; 1 for infected; 0 for removed
  # set up plot
  plot(c(1,nrow(X)), c(1,ncol(X)), type = "n", xlab = "", ylab = "")
  # main loop
  burning <- TRUE
  while (burning) {
    burning <- FALSE
    # check if pausing between updates
    if (pausing) {
      input <- readline("hit any key to continue")
      forest.fire.plot(X)
      
    }
    # update
    B <- X
    for (i in 1:nrow(X)) {
      for (j in 1:ncol(X)) {
        if (X[i, j] == 2) {
          if (runif(1) > (1 - a)^neighbours(X, i, j)) {
            B[i, j] <- 1
          }
        } else if (X[i, j] == 1) {
          burning <- TRUE
          if (runif(1) < b) {
            B[i, j] <- 0
          }
        }
      }
    }
    X <- B
  }
  return(X)
}

#---------------------- Code to Test the solution --------------------------- #
# set.seed(3)
# X <- matrix(2, 21, 21)
# X[11, 11] <- 1
# # big fires
# result <- forest.fire(X, .2, .4, FALSE)
# # # # Tests to be run when modifying the code
# runTest <- function(result) {
#    stopifnot(result[1, 1] ==  2)
#    stopifnot(result[7,12] == 2)
#    stopifnot(result[11,19] == 0)
#    stopifnot(result[15,8] == 2)
#    stopifnot(result[15,7] == 0)
#    stopifnot(result[21,17] == 0)
# }
# runTest(result)

