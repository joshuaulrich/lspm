\name{probProfit}
\Rdversion{1.1}
\alias{probProfit}
\title{ Probability of Profit }
\description{
  Functions to compute the probability of profit
}
\usage{
  probProfit(lsp, target, horizon, error=0.001, sigma=3, snow=NULL)
}
\arguments{
  \item{lsp}{ A \code{lsp} object. }
  \item{target}{ The profit target. }
  \item{horizon}{ The number of holding periods. }
  \item{error}{ Error level to determine permutation sample size. }
  \item{sigma}{ Confidence level to determine permutation sample size. }
  \item{snow}{ An optional \code{snow} object. }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  The probability of profit
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
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
