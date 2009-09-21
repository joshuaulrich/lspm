\name{riskFunctions}
\Rdversion{1.1}
\alias{riskFunctions}
\alias{riskDrawdown}
\alias{riskRuin}
\title{ Risk Functions }
\description{
  Functions to compute the risk of drawdown and risk of ruin
}
\usage{
  riskDrawdown(lsp, DD, horizon, calc.max=10, error=0.001, sigma=3, snow=NULL)
  riskRuin(lsp, DD, horizon, calc.max=10, error=0.001, sigma=3, snow=NULL)
}
\arguments{
  \item{lsp}{ A \code{lsp} object. }
  \item{DD}{ The maximum drawdown (\code{riskDrawdown}) or percentage
    of starting equity at which ruin occurs (\code{riskRuin}). }
  \item{horizon}{ The number of holding periods. }
  \item{calc.max}{ If \code{horizon} is greater than \code{calc.max}
    then the risk function will be calculated for \code{horizon=1:calc.max}
    and the risk at \code{horizon} will be estimated using those values. }
  \item{error}{ Error level to determine permutation sample size. }
  \item{sigma}{ Confidence level to determine permutation sample size. }
  \item{snow}{ An optional \code{snow} object. }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  The risk of drawdown or ruin
}
\references{
  Ralph Vince, The Handbook of Portfolio Mathematics\cr
  Ralph Vince, The Leverage Space Trading Model
}
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
