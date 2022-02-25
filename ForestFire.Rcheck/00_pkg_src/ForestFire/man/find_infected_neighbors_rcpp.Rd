\name{find_infected_neighbors_rcpp}
\alias{find_infected_neighbors_rcpp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
find_infected_neighbors_rcpp
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
find_infected_neighbors_rcpp(infection_matrix, i, j)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{infection_matrix}{: main infection_matrix that maps an individual and 
its current state state (unburnt, on fire or burnt out)}

\item{i}{: current row in the  infection_matrix to detect neighbors}

\item{j}{: current column in the  infection_matrix to detect neighbors}{
%%     ~~Describe \code{x} here~~
}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
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
