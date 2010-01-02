\name{riskFunctions}
\Rdversion{1.1}
\alias{riskFunctions}
\alias{probDrawdown}
\alias{probRuin}
\title{ Risk Probability Functions }
\description{
  Functions to compute the probability of drawdown and ruin
}
\usage{
  probDrawdown(lsp, DD, horizon, calc.max=10, error=0.001, sigma=3, snow=NULL)
  probRuin(lsp, DD, horizon, calc.max=10, error=0.001, sigma=3, snow=NULL)
}
\arguments{
  \item{lsp}{ A \code{lsp} object. }
  \item{DD}{ The maximum drawdown (\code{probDrawdown}) or percentage
    of starting equity at which ruin occurs (\code{probRuin}). }
  \item{horizon}{ The number of holding periods. }
  \item{calc.max}{ If \code{horizon} is greater than \code{calc.max}
    then the probability function will be calculated for \code{horizon=1:calc.max}
    and the probability at \code{horizon} will be estimated using those values. }
  \item{error}{ Error level to determine permutation sample size. }
  \item{sigma}{ Confidence level to determine permutation sample size. }
  \item{snow}{ An optional \code{snow} object. }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  The probability of drawdown or ruin
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
