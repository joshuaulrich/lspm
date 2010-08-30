\name{jointProbTable}
\Rdversion{1.1}
\alias{jointProbTable}
\title{ Joint Probability Table }
\description{
  Create a joint probability table for a series of trades
}
\usage{
  jointProbTable(x, n=3, FUN=median, ...)
}
\arguments{
  \item{x}{ A vector, data.frame, or matrix. }
  \item{n}{ A number of bins to create for each column in
    \code{x}.  You can use a vector of length \code{NCOL(x)}
    to specify a different number of bins for each column. }
  \item{FUN}{ The function used to assign a value to each bin. }
  \item{\dots}{ Parameters to be passed to \code{FUN}. }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A \code{lsp} object.
}
\references{
  Vince, Ralph (2007) \emph{The Handbook of Portfolio Mathematics}.
  New York: John Wiley & Sons, Inc.\cr
  Vince, Ralph (2009) \emph{The Leverage Space Trading Model}.
  New York: John Wiley & Sons, Inc.}
\author{ Joshua Ulrich }
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
  
  df <- data.frame(x=rnorm(10),y=rnorm(10))
  jptMedian <- jointProbTable(df, n=c(2,3))
  jptMean <- jointProbTable(df, FUN=mean)
  jptMin  <- jointProbTable(df, FUN=min)
  jptMax  <- jointProbTable(df, FUN=max)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
