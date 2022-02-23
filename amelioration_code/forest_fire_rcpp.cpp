#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

int find_infected_neighbors(IntegerMatrix infection_matrix, int i, int j)  {
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
// #' 
// #'  
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
// #plot(c(1,nrow(infection_matrix)), c(1,ncol(infection_matrix)), type = "n", xlab = "", ylab = "")
// #forest_fire_plot(infection_matrix)
// # main loop
for (int iterator=1; iterator < 21; ++iterator) {
    //# check if pausing between updates
    // if (pausing) {
    //   input <- readline("hit any key to continue")
    // }
// make a copy of the infection_matrix
      IntegerMatrix infection_matrix_copy= clone(infection_matrix);
    //#iterate though the infection_matrix 
      for (int i = 0; i < infection_matrix.nrow() ; ++i) {
        for (int j = 0; j < infection_matrix.ncol()  ; ++j) {
          //#if an individual is unburt
          if(infection_matrix(i, j) == 2) {
            //#check number of neighbors are infected 
            int n_infected = find_infected_neighbors(infection_matrix, i, j);
            //#if the probability is bigger than the probability remaining uninfected (1 alpha)^n_infected
            //
            if( runif(1)[0] > pow(1 - alpha, n_infected)) {
              //#it changes status for on fire
              infection_matrix_copy(i, j) = 1;
            }
            //#else if an individual is on fire
          } 
          else if(infection_matrix(i, j) == 1) {
            //#the forest will continue burning 
            //#if probability if less than beta an individual is removed
            if(runif(1)[0] < beta) {
              //#it will be burn out
              infection_matrix_copy(i, j) = 0;
            }
          }
        }
      }
      infection_matrix = clone(infection_matrix_copy);
// # plot
// #forest_fire_plot(infection_matrix)
  }
  return infection_matrix;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
set.seed(3)
# # #initialize a infection_matrix of 21x21 with only 2 values (susceptible people)
# infection_matrix <- matrix(2, 21, 21)
# # # #in the central position of the infection_matrix set a 1 value
# infection_matrix[11, 11] <- 1
# result <- forest_fire_rcpp(infection_matrix, .2, .4, FALSE)
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

*/
