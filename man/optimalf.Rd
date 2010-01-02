\name{optimalf}
\Rdversion{1.1}
\alias{optimalf}
\title{ Optimal f }
\description{
  Find optimal f for a set of trades
}
\usage{
  optimalf(lsp, constrFun=NULL, constrVal=NULL, ...)
}
\arguments{
  \item{lsp}{ A \code{lsp} object. }
  \item{constrFun}{ A string naming the constraint function. }
  \item{constrVal}{ The value of the constraint function that should
    not be exceeded. }
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
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
