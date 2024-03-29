#include <Rcpp.h>
using namespace Rcpp;

// ## File: forest_file_rcpp.cpp
// ## Students : Lia Furtado and Hugo Vinsion
// ## Description : Projet Parallel Computing 2021 - Code improvements
// # forest fire simulation - Chapter 21 (21.2.3 Forest fire model)
// 
// ## Description : Improved code commented and more memory efficient
// 
// ## Date : 21 February 2022


// [[Rcpp::export]]
int find_infected_neighbors_rcpp(IntegerMatrix infection_matrix, int i, int j)  {
  // #' Function to find for the point infection_matrix[i,j] the neighbors that were infected 
  // #' from all the 8 edges from a point in the matrix
  // #' For point (x, y) the points (x + 1, y + 1), (x + 1, y), (x + 1, y + 1),
  // #'(x, y  + 1), (x, y + 1), (x + 1, y + 1), (x + 1, y), and (x + 1, y + 1) are considered neighbors.
  // #'
  // #' @param infection_matrix : main infection_matrix that maps an individual and 
  // #' its current state state (unburnt, on fire or burnt out)  
  // #' @param i: current row in the  infection_matrix to detect neighbors
  // #' @param j: current column in the  infection_matrix to detect neighbors
  // #'
  // #' @return number_infected: number of infected individuals after checking the 8 neighbors of a point
  
  int number_infected = 0;
  int Ncol = infection_matrix.ncol();
  int Nrow = infection_matrix.nrow();
  if (i > 0) {
    if (j > 0) {
      number_infected = number_infected + (infection_matrix(i-1, j-1) == 1);
    }
    number_infected = number_infected + (infection_matrix(i-1, j) == 1);
    if (j < Ncol - 1) {
      number_infected = number_infected + (infection_matrix(i-1, j+1) == 1);
    }
  }
  if (j > 0) {
    number_infected = number_infected + (infection_matrix(i, j-1) == 1);
  }
  number_infected = number_infected + (infection_matrix(i, j) == 1);
  if (j < Ncol - 1) {
    number_infected = number_infected + (infection_matrix(i, j+1) == 1);
  }
  if (i < Nrow - 1) {
    if (j > 0) {
      number_infected = number_infected + (infection_matrix(i+1, j-1) == 1);
    }
    number_infected = number_infected + (infection_matrix(i+1, j) == 1);
    if (j < Ncol - 1) {
      number_infected = number_infected + (infection_matrix(i+1, j+1) == 1);
    }
  }
  return number_infected;
}

// [[Rcpp::export]]
IntegerMatrix forest_fire_rcpp(IntegerMatrix infection_matrix, double alpha, double beta, bool pausing) {
// #' Function to simulate a forest fire epidemic model
// #' For infection_matrix[i, j] = 2, person is unburnt; 1 for on fire; 0 for burnt out.
// #' An on fire individual can only infect unburnt individuals if they are neighbors.
// #' @param infection_matrix : main infection_matrix that maps an individual and 
// #' its current state state (unburnt, on fire or burnt out)  
// #' @param alpha: probability of an infected individual of infecting each of its susceptible neighbors
// #' @param beta: probability of an individual that was on fire and had a chance of 
// #' affecting people of getting unburnt 
// #' @param pausing 
// #'
// #' @return infection_matrix: returns the updated infection_matrix after the simulation
// #'

// # main loop
  bool burning = TRUE;
  while (burning) {
    burning = FALSE;
    //# check if pausing between updates
    if (pausing) {
      //input <- readline("hit any key to continue")
    }
    //make a copy of the infection_matrix
      IntegerMatrix infection_matrix_copy= clone(infection_matrix);
    //#iterate though the infection_matrix 
      for (int i = 0; i < infection_matrix.nrow() ; ++i) {
        for (int j = 0; j < infection_matrix.ncol()  ; ++j) {
          //#if an individual is unburt
          if(infection_matrix(i, j) == 2) {
            //#check number of neighbors are infected 
            int n_infected = find_infected_neighbors_rcpp(infection_matrix, i, j);
            //#if the probability is bigger than the probability remaining uninfected (1 - alpha)^n_infected
            if( runif(1)[0] > pow(1 - alpha, n_infected)) {
              //#it changes status for on fire
              infection_matrix_copy(i, j) = 1;
            }
            //#else if an individual is on fire
          } 
          else if(infection_matrix(i, j) == 1) {
            //#the forest will continue burning 
            burning = TRUE;
            //#if probability if less than beta an individual is removed
            if(runif(1)[0] < beta) {
              //#it will be burn out
              infection_matrix_copy(i, j) = 0;
            }
          }
        }
      }
      infection_matrix = clone(infection_matrix_copy);
}
  return infection_matrix;
}


/*** R
# set.seed(3)
# infection_matrix <- matrix(2, 21, 21)
# infection_matrix[11, 11] <- 1
# # big fires
# result <- forest_fire_rcpp(infection_matrix, .2, .4, FALSE)
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
*/
