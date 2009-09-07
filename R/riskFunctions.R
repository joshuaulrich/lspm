riskRuin <- function(DD, horizon, error=0.001, sigma=3, f, trades, probs=NULL, maxLoss=NULL) {

  # This should work on multiple columns
  hpr <- as.matrix(-f * trades / maxLoss)
  
  # In case there are multiple columns
  NR <- NROW(hpr)
  res <- sapply(1:NR, function(i) (1+sum(hpr[i,])))

  nsamp <- ( (sigma/error) ^ 2 ) * 0.25
  nperm <- NR ^ horizon
  
  failProb <- sumProb <- 0

  # This will be uncommented once I finish my R-based
  # permutation finder. -- JMU
  #if(nperm < nsamp) {

  #} else {
    permCount <- 1
    while( permCount <= nsamp && permCount <= nperm ) {
        
      perm <- sample.int(NR, horizon, replace=TRUE)
      hprPerm <- res[perm]
      probPerm <- prod( probs[perm] )

      cumhpr <- cumprod(hprPerm)
      cumhpr <- cumhpr-(1-DD)
      if( any(cumhpr <= 0) ) {
        failProb <- failProb + probPerm
      }
      sumProb <- sumProb + probPerm
      permCount <- permCount + 1
    }
  #}

  out <- failProb / sumProb

  return(out)
}

riskDrawdown <- function(DD, horizon, error=0.001, sigma=3, f, trades, probs=NULL, maxLoss=NULL) {

  # This should work on multiple columns
  hpr <- as.matrix(-f * trades / maxLoss)
  
  # In case there are multiple columns
  NR <- NROW(hpr)
  res <- sapply(1:NR, function(i) (1+sum(hpr[i,])))

  nsamp <- ( (sigma/error) ^ 2 ) * 0.25
  nperm <- NR ^ horizon
  
  failProb <- sumProb <- 0

  # This will be uncommented once I finish my R-based
  # permutation finder. -- JMU
  #if(nperm < nsamp) {

  #} else {
    permCount <- 1
    while( permCount <= nsamp && permCount <= nperm ) {
        
      perm <- sample.int(NR, horizon, replace=TRUE)
      hprPerm <- res[perm]
      probPerm <- prod( probs[perm] )

      cumhpr <- cumprod(hprPerm)
      cumhpr <- cumhpr/cummax(cumhpr)-(1-DD)
      if( any(cumhpr <= 0) ) {
        failProb <- failProb + probPerm
      }
      sumProb <- sumProb + probPerm
      permCount <- permCount + 1
    }
  #}

  out <- failProb / sumProb

  return(out)
}

RX.asymptote <- function(RX, horizon, RD=FALSE) {

  objFun <- function(params, RX, horizon, RD) {
    if(RD) {
      params[1] <- 1
    }
    res <- params[1] - params[2] * exp( -params[3] * horizon )
    res <- sum( (RX - res)^2 )
    return(res)
  }

  out <- DEoptim(objFun, lower=matrix(c(0,0,0)), upper=matrix(c(1,1,1)),
    RX=D[,2], horizon=D[,1], RD=RD)

  return(out)
}

