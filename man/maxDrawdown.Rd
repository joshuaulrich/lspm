\name{maxDrawdown}
\Rdversion{1.1}
\alias{maxDrawdown}
\title{ Maximum Drawdown }
\description{
  Calculate the maximum drawdown of a lsp object
}
\usage{
  maxDrawdown(lsp, sort=FALSE, geometric=TRUE)
}
\arguments{
  \item{lsp}{ A \code{lsp} object. }
  \item{sort}{ If \code{FALSE} (the default), maximum drawdown is
    calculated using the returns in the order provided; if \code{TRUE},
    returns are sorted to reflect the worst possible maximum drawdown. }
  \item{geometric}{ Use log returns? }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  The maximum drawdown percentage.
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
