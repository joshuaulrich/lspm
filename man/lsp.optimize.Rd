\name{lsp.optimize}
\Rdversion{1.1}
\alias{lsp.optimize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lsp.optimize(jpt, DEcontrol = list(), MAXCYCLES = 18, sigmas = 5, error.size = 0.01, use.drawdown = TRUE, acceptable.percent = 0.2, horizon = 30, b = 0.8, max.probprofit = TRUE, target.return = 0.02, zminusmin = -1, zplusmin = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jpt}{
%%     ~~Describe \code{jpt} here~~
}
  \item{DEcontrol}{
%%     ~~Describe \code{DEcontrol} here~~
}
  \item{MAXCYCLES}{
%%     ~~Describe \code{MAXCYCLES} here~~
}
  \item{sigmas}{
%%     ~~Describe \code{sigmas} here~~
}
  \item{error.size}{
%%     ~~Describe \code{error.size} here~~
}
  \item{use.drawdown}{
%%     ~~Describe \code{use.drawdown} here~~
}
  \item{acceptable.percent}{
%%     ~~Describe \code{acceptable.percent} here~~
}
  \item{horizon}{
%%     ~~Describe \code{horizon} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
  \item{max.probprofit}{
%%     ~~Describe \code{max.probprofit} here~~
}
  \item{target.return}{
%%     ~~Describe \code{target.return} here~~
}
  \item{zminusmin}{
%%     ~~Describe \code{zminusmin} here~~
}
  \item{zplusmin}{
%%     ~~Describe \code{zplusmin} here~~
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
function (jpt, DEcontrol = list(), MAXCYCLES = 18, sigmas = 5, 
    error.size = 0.01, use.drawdown = TRUE, acceptable.percent = 0.2, 
    horizon = 30, b = 0.8, max.probprofit = TRUE, target.return = 0.02, 
    zminusmin = -1, zplusmin = -1) 
{
    RRDR <- .jnew("com/leveragespacemodel/RRDRTestRaw", as.integer(MAXCYCLES), 
        sigmas, error.size, use.drawdown, acceptable.percent, 
        as.integer(horizon), b)
    RRDRPP <- .jnew("com/leveragespacemodel/RRDRPPTestRaw", as.integer(MAXCYCLES), 
        sigmas, error.size, use.drawdown, acceptable.percent, 
        as.integer(horizon), b, target.return)
    if (max.probprofit) {
        d <- (dim(jpt)[2] - 1) + 2
    }
    else {
        d <- (dim(jpt)[2] - 1)
    }
    f <- rep(0, d)
    lower <- rep(0, d)
    if (max.probprofit) {
        upper <- c(rep(1, (d - 2)), -zminusmin, -zplusmin)
    }
    else {
        upper <- rep(1, d)
    }
    de <- DEoptim(.obj.function, lower = lower, upper = upper, 
        control = DEcontrol, jpt, max.probprofit, RRDR, RRDRPP)
    bestf <- de$optim$bestmem
    msnames <- colnames(jpt)[-1]
    if (length(msnames) == 1) {
        msnames <- .jarray(msnames, "S")
    }
    if (length(bestf) == 1) {
        bestf <- .jarray(bestf, "D")
    }
    probs <- jpt[, 1]
    plays <- as.matrix(jpt[, -1])
    d <- dim(plays)
    if (max.probprofit) {
        .jcall(RRDRPP, "Z", "test", msnames, bestf, as.vector(probs), 
            as.vector(t(plays)), as.integer(d))
        probprofitathorizon <- .jfield(RRDRPP, "D", "probprofitathorizon")
        percentrdhit <- .jfield(RRDRPP, "D", "percentRDhit")
        zminus <- bestf[length(bestf) - 1]
        zplus <- bestf[length(bestf)]
        f <- bestf[-((length(bestf) - 1):length(bestf))]
        cat("\nf =", paste(rbind(msnames, f)), "\nz- =", -zminus, 
            "\nz+ =", -zplus, "\nPP(", target.return, ",", horizon, 
            ") =", probprofitathorizon)
        if (use.drawdown) {
            cat("\nRD(", b, ",", acceptable.percent, ",", horizon, 
                ") =", percentrdhit, "\n")
        }
        else {
            cat("\nRR(", b, ",", acceptable.percent, ",", horizon, 
                ") =", percentrdhit, "\n")
        }
    }
    else {
        .jcall(RRDR, "Z", "test", msnames, bestf, as.vector(probs), 
            as.vector(t(plays)), as.integer(d))
        percentrdhit <- .jfield(RRDR, "D", "percentRDhit")
        gab <- .jfield(RRDR, sig = NULL, "gab")
        asymptote <- .jfield(gab, "D", "asymptote")
        coefficient <- .jfield(gab, "D", "coefficient")
        exponent <- .jfield(gab, "D", "exponent")
        g <- .findg(bestf, jpt, RRDR)
        cat("\nf =", paste(rbind(msnames, bestf)), "\nG =", g)
        if (MAXCYCLES < horizon) {
            cat("\nasymptote =", asymptote, "\ncoefficient =", 
                coefficient, "\nexponent =", exponent)
        }
        if (use.drawdown) {
            cat("\nRD(", b, ",", acceptable.percent, ",", horizon, 
                ") =", percentrdhit, "\n")
        }
        else {
            cat("\nRR(", b, ",", acceptable.percent, ",", horizon, 
                ") =", percentrdhit, "\n")
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
