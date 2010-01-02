\name{HPR}
\Rdversion{1.1}
\alias{HPR}
\title{ Holding Period Return }
\description{
  Find holding period return for a set of events
}
\usage{
  HPR(lsp, portfolio=FALSE)
}
\arguments{
  \item{lsp}{ A \code{lsp} object. }
  \item{portfolio}{ If \code{TRUE}, only the total portfolio HPR
    is returned. }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A vector or matrix containing the holding period returns
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

\seealso{
  \code{\link{GHPR}}
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
