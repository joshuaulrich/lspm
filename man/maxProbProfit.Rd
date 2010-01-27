\name{maxProbProfit}
\Rdversion{1.1}
\alias{maxProbProfit}
\title{ Maximize the Probability of Profit }
\description{
  Maximize the Probability of Profit
}
\usage{
  maxProbProfit(lsp, target, horizon, constrFun=NULL, constrVal=NULL,
    zmin=-1, ...)
}
\arguments{
  \item{lsp}{ A \code{lsp} object. }
  \item{target}{ The profit target. }
  \item{horizon}{ The number of holding periods. }
  \item{constrFun}{ A string naming the constraint function. }
  \item{constrVal}{ The value of the constraint function that should
    not be exceeded. }
  \item{zmin}{ A value or vector of minimum values for the two
    martingale exponents.  If \code{zmin} is a vector, the first
    (second) element is used when portfolio equity is below (above)
    \code{target}. \code{zmin} must be <= -1 and >= 0. }
  \item{\dots}{ Parameters to be passed to \code{constrFun}. }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{f}{ Initial f values. }
  \item{z}{ Martingale z values.  The first (second) exponent is used
    when portfolio equity is below (above) the target value. }
  \item{profitProb}{ Maximum probability of profit. }
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
