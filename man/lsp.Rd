\name{lsp}
\Rdversion{1.1}
\alias{lsp}
\title{ Create a Leverage Space Portfolio Object }
\description{
  Leverage Space Portfolio object constructor
}
\usage{
  lsp(events, probs=NULL, f=NULL, maxLoss=NULL, z=NULL)
}
\arguments{
  \item{events}{ A matrix-like object containing event series
    (scenario spectrums). }
  \item{probs}{ A vector of probabilities associated with each
    outcome for all scenario spectrums.  If \code{NULL}, each
    outcome is assigned the same probability. }
  \item{f}{ A vector of f values for each scenario spectrum.  If
    \code{NULL}, each scenario spectrum is assigned an f=0.1. }
  \item{maxLoss}{ A vector of maximum losses for each scenario
    spectrum.  If \code{NULL}, each scenario spectrum is assigned
    its minimum outcome value. }
  \item{z}{ A vector of (2) martingale exponents and (1) target
    return value.  The first (second) exponent is used when
    portfolio equity is below (above) the target value. }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  An S3 object of class \code{lsp}.
}
%\references{
%  Ralph Vince, The Handbook of Portfolio Mathematics
%}
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
