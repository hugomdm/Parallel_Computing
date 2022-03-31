# Function plot the spread of fire representation with points 
#
# @param infection_matrix main infection_matrix that maps an individual and 
# its current state state (unburnt, on fire or burnt out)  
#
# @return
# @export
#
# @examples
# 
# 
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