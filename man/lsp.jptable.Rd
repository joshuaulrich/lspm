\name{lsp.jptable}
\Rdversion{1.1}
\alias{lsp.jptable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lsp.jptable(lsp.portfolio)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lsp.portfolio}{
%%     ~~Describe \code{lsp.portfolio} here~~
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
function (lsp.portfolio) 
{
    order.lsp.portfolio <- do.call(order, as.data.frame(lsp.portfolio))
    equal.to.previous <- rowSums(lsp.portfolio[tail(order.lsp.portfolio, 
        -1), ] != lsp.portfolio[head(order.lsp.portfolio, -1), 
        ]) == 0
    tf.runs <- rle(equal.to.previous)
    probability <- c(1, unlist(mapply(function(x, y) if (y) x + 
        1 else (rep(1, x)), tf.runs$length, tf.runs$value)))
    probability <- probability[c(diff(probability) <= 0, TRUE)]
    probability <- probability/nrow(lsp.portfolio)
    unique.rows <- which(c(TRUE, !equal.to.previous))
    jpt <- cbind(probability, lsp.portfolio[order.lsp.portfolio[unique.rows], 
        , drop = F])
    rownames(jpt) <- NULL
    return(jpt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
