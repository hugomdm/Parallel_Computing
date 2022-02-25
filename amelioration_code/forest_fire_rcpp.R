library(Rcpp)



Rcpp::cppFunction('int find_infected_neighbors(IntegerMatrix infection_matrix, int i, int j)  {
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
                  }')


Rcpp::cppFunction('IntegerMatrix forest_fire_rcpp(IntegerMatrix infection_matrix, double alpha, double beta, bool pausing) {
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
                                int n_infected = find_infected_neighbors(infection_matrix, i, j);
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
                  }')

set.seed(3)
infection_matrix <- matrix(2, 21, 21)
infection_matrix[11, 11] <- 1
# big fires
result <- forest_fire_rcpp(infection_matrix, .2, .4, FALSE)
# # # Tests to be run when modifying the code
runTest <- function(result) {
  stopifnot(result[1, 1] ==  2)
  stopifnot(result[7,12] == 2)
  stopifnot(result[11,19] == 0)
  stopifnot(result[15,8] == 2)
  stopifnot(result[15,7] == 0)
  stopifnot(result[21,17] == 0)
}
runTest(result)