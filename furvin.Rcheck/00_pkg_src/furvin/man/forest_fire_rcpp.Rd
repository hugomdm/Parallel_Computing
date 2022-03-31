\name{forest_fire_rcpp}
\alias{forest_fire_rcpp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate Forest Fire using rcpp techniques 
}
\description{
Function to simulate a forest fire epidemic model
For infection_matrix[i, j] = 2, person is unburnt; 1 for on fire; 0 for burnt out.
An on fire individual can only infect unburnt individuals if they are neighbors.
}
\usage{
forest_fire_rcpp(infection_matrix, alpha,  beta, pausing)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{infection_matrix}{: main infection_matrix that maps an individual and 
its current state state (unburnt, on fire or burnt out)}

\item{pausing}{boolean value to print the plot of the forest or not}

\item{alpha:}{probability of an infected individual of infecting each of its susceptible neighbors}

\item{beta:}{probability of an individual that was on fire and had a chance of 
affecting people of getting unburnt}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
set.seed(3)
infection_matrix <- matrix(2, 21, 21)
infection_matrix[11, 11] <- 1
result = forest_fire_rcpp(infection_matrix, .2, .4, FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
