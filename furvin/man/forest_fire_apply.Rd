% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forest_fire_apply.R
\name{forest_fire_apply}
\alias{forest_fire_apply}
\title{forest_fire_apply: Simulate Forest Fire using apply technique}
\usage{
forest_fire_apply(infection_matrix, alpha, beta, pausing = FALSE)
}
\arguments{
\item{infection_matrix}{: main infection_matrix that maps an individual and 
its current state state (unburnt, on fire or burnt out)}

\item{alpha}{: probability of an infected individual of infecting each of its susceptible neighbors}

\item{beta}{: probability of an individual that was on fire and had a chance of 
affecting people of getting unburnt}

\item{pausing}{: boolean value to print the plot of the forest or not}
}
\value{
infection_matrix: returns the updated infection_matrix after the simulation
}
\description{
Function to simulate a forest fire epidemic model
For infection_matrix[i, j] = 2, person is unburnt; 1 for on fire; 0 for burnt out.
An on fire individual can only infect unburnt individuals if they are neighbors.
}
\examples{
set.seed(3)
infection_matrix <- matrix(2, 21, 21)
infection_matrix[11, 11] <- 1
result = forest_fire_apply(infection_matrix, .2, .4, FALSE)

}
