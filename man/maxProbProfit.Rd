\name{maxProbProfit}
\Rdversion{1.1}
\alias{maxProbProfit}
\title{ Maximize the Probability of Profit }
\description{
  Maximize the Probability of Profit
}
\usage{
  maxProbProfit(lsp, target, horizon, constrFun=NULL, constrVal=NULL,
    zmin=-1, margin=NULL, equity=NULL, upper, lower, ...)
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
  \item{margin}{ A vector of inital margin values for each event series. }
  \item{equity}{ Current account equity. }
  \item{upper}{ Upper f-value bounds (recycled, if necessary). }
  \item{lower}{ Lower f-value bounds (recycled, if necessary). } 
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
\examples{
 \dontrun{
  data(port)
  
  # DEoptim parameters (see ?deoptim)
  DEctrl <- list(itermax=11, trace=1)
  
  # Unconstrained maximum probability of profit
  res <- maxProbProfit(port, 1e-6, 4, control=DEctrl)

  # Margin-constrained maximum probability of profit
  res <- maxProbProfit(port, 1e-6, 4, control=DEctrl,
    margin=-port$maxLoss*5, equity=1e6)

  # Initial population
  ipop <- cbind(runif(50,0,0.01),runif(50,0,0.01),runif(50,0,0.01),
    runif(50,-1,-0.8),runif(50,-1,-0.8))
  
  # DEoptim parameters (see ?deoptim)
  DEctrl <- list(itermax=11, trace=1, initial=ipop)
  
  # Drawdown-constrained maximum probability of profit
  res <- maxProbProfit(port, 1e-6, 4, probDrawdown, 0.1,
    DD=0.2, calc.max=4, control=DEctrl)

  # Create snow socket cluster for two cores
  library(snow)
  clust <- makeSOCKcluster(2)

  # Drawdown-constrained maximum probability of profit using two cores
  res <- maxProbProfit(port, 1e-6, 12, probDrawdown, 0.1,
    DD=0.2, calc.max=4, snow=clust, control=DEctrl)
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
