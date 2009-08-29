\name{GHPR}
\Rdversion{1.1}
\alias{GHPR}
\title{ Geometric Holding Period Return }
\description{
  Find geometric holding period return for a set of trades
}
\usage{
  GHPR(f, trades, probs=NULL, maxLoss=NULL)
}
\arguments{
  \item{f}{ f value to use (need not be optimal f). \code{f} must
    have length equal to the number of columns of \code{trades} }
  \item{trades}{ A univariate or multivariate series of trade
    results (either dollar amounts or returns) }
  \item{probs}{ A univariate series of probablities corresponding to
    each element in \code{trades} }
  \item{maxLoss}{ A replacement for the actual maximum loss in
    \code{trades}. \code{maxLoss} must have length equal to the
    number of columns of \code{trades} }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Geometric holding period return
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
