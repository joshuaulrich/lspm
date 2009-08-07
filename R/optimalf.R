optimalf <- function(trades, probs=NULL, maxLoss=NULL, revNegative=FALSE) {

  if (is.null(probs)) {
    probs <- rep(1/NROW(trades),NROW(trades))
  } else {
    if (NROW(trades) != NROW(probs)) {
      stop("'trades' and 'probs' must be same length")
    }
  }

  REV <- FALSE
  if (sum(trades*probs) < 0) {
    if (revNegative) {
      trades <- -1*trades
      REV <- TRUE
    } else {
      stop("'trades' (and 'probs') has expected value <= 0")
    }
  }

  if (is.null(maxLoss)) maxLoss <- min(trades)
  if (maxLoss >= 0) stop("must have at least one negative trade")

  gEV <- function(f, trades, probs, maxLoss) {
   prod( (1+f*(-trades/maxLoss)) ^ probs ) ^ (1/sum(probs))
  }

  res <- optimize(gEV, interval=c(0,1), trades=trades,
                 probs=probs, maxLoss=maxLoss, maximum=TRUE)
  names(res) <- c("f","G")

  res <- c(res, maxLoss=maxLoss, rev=REV)

  return(res)
}
