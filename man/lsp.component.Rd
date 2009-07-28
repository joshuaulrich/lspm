\name{lsp.component}
\Rdversion{1.1}
\alias{lsp.component}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lsp.component(x, num.bins)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{num.bins}{
%%     ~~Describe \code{num.bins} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, num.bins) 
{
    gen.scenarios <- function(x) {
        labs <- levels(x)
        lower <- as.numeric(sub("\\((.+),.*", "\\1", labs))
        upper <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", labs))
        data.frame(lower = lower, mid = (upper + lower)/2, upper = upper)
    }
    scenarios <- gen.scenarios(cut(x, num.bins))
    c <- as.numeric(as.character(cut(x, num.bins, labels = scenarios$mid)))
    return(c)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
