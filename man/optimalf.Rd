\name{optimalf}
\Rdversion{1.1}
\alias{optimalf}
\title{ Optimal f }
\description{
  Find optimal f for a set of trades
}
\usage{
  optimalf(trades, probs=NULL, maxLoss=NULL, revNegative=FALSE)
}
\arguments{
  \item{trades}{ A univariate series of trade results (either dollar 
    amounts or returns) }
  \item{probs}{ A univariate series of probablities corresponding to
    each element in \code{trades} }
  \item{maxLoss}{ A replacement for the actual maximum loss in
    \code{trades} }
  \item{revNegative}{ Should we reverse the sign for each element in
    \code{trades} to achive a positive expected value? }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{f}{ Optimal f }
  \item{G}{ Growth at optimal f }
  \item{rev}{ Were trades reversed to achive positive expectancy? }
}
\references{
  Ralph Vince, The Handbook of Portfolio Mathematics
}
\author{ Joshua Ulrich }
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
