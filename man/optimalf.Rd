\name{optimalf}
\Rdversion{1.1}
\alias{optimalf}
\title{ Optimal f }
\description{
  Find optimal f for a set of trades
}
\usage{
  optimalf(lsp, constrFun=NULL, constrVal=NULL,
    margin=NULL, equity=NULL, upper, lower, ...)
}
\arguments{
  \item{lsp}{ A \code{lsp} object. }
  \item{constrFun}{ A string naming the constraint function. }
  \item{constrVal}{ The value of the constraint function that should
    not be exceeded. }
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
  \item{f}{ Optimal f }
  \item{G}{ GHPR at the optimal f }
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
  data(port)
  
  # DEoptim parameters (see ?DEoptim)
  DEctrl <- list(NP=30, itermax=100)

  # Unconstrainted Optimal f
  res <- optimalf(port, control=DEctrl)

  # Margin-constrainted Optimal f
  resMargin <- optimalf(port, control=DEctrl,
    equity=1e5, margin=-port$maxLoss*2)

 \dontrun{
  # Ruin-constrained Optimal f
  resRuin <- optimalf(port, probRuin, 0.1, DD=0.2,
    horizon=4, control=DEctrl)

  # Drawdown-constrained Optimal f
  resDrawdown <- optimalf(port, probDrawdown, 0.1, DD=0.2,
    horizon=4, control=DEctrl)

  # Create snow socket cluster for two cores
  library(snow)
  clust <- makeSOCKcluster(2)

  # Drawdown-constrained Optimal f using two cores
  resSnow <- optimalf(port, probDrawdown, 0.1, DD=0.2,
    horizon=4, snow=clust, control=DEctrl)
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
