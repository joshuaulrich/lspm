#
#   LSPM: The Leverage Space Portfolio Modeler
#
#   Copyright (C) 2009  Soren MacBeth, Joshua Ulrich, and Ralph Vince
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

riskRuin <- function(DD, horizon, error=0.001, sigma=3, f, trades, probs=NULL, maxLoss=NULL) {

  # All this checking should go into the lsp class constructor
  if(length(f) != NCOL(trades)) {
    stop("length(f) must equal ncol(trades)")
  }

  if(is.null(maxLoss)) {
    maxLoss <- sapply(1:NCOL(trades), function(i) min(as.matrix(trades)[,i]))
  }
  if(length(maxLoss) != NCOL(trades)) {
    stop("length(maxLoss) must equal ncol(trades)")
  }
  if(any(maxLoss >= 0)) {
    stop("all 'trades' columns must have at least one negative trade")
  }

  if(is.null(probs)) {
    probs <- rep(1/NROW(trades),NROW(trades))
  } else {
    if(NROW(trades) != NROW(probs)) {
      stop("'trades' and 'probs' must be same length")
    }
  }

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
  if(nperm < nsamp) {
    for( i in 1:nperm ) {
      perm <- .nPri(NR, horizon, i, replace=TRUE)
      hprPerm <- res[perm]
      probPerm <- prod( probs[perm] )

      cumhpr <- cumprod(hprPerm)
      cumhpr <- cumhpr-(1-DD)
      if( any(cumhpr <= 0) ) {
        failProb <- failProb + probPerm
      }
      sumProb <- sumProb + probPerm
    }
  } else {
    permCount <- 1
    while( permCount <= nsamp && permCount <= nperm ) {
        
      perm <- sample.int(NR, horizon, replace=TRUE)
      hprPerm <- res[perm]
      probPerm <- prod( probs[perm] )

      cumhpr <- cumprod(c(1,hprPerm))
      cumhpr <- cumhpr-(1-DD)
      if( any(cumhpr <= 0) ) {
        failProb <- failProb + probPerm
      }
      sumProb <- sumProb + probPerm
      permCount <- permCount + 1
    }
  }

  out <- failProb / sumProb

  return(out)
}

riskDrawdown <- function(DD, horizon, error=0.001, sigma=3, f, trades, probs=NULL, maxLoss=NULL) {

  # All this checking should go into the lsp class constructor
  if(length(f) != NCOL(trades)) {
    stop("length(f) must equal ncol(trades)")
  }

  if(is.null(maxLoss)) {
    maxLoss <- sapply(1:NCOL(trades), function(i) min(as.matrix(trades)[,i]))
  }
  if(length(maxLoss) != NCOL(trades)) {
    stop("length(maxLoss) must equal ncol(trades)")
  }
  if(any(maxLoss >= 0)) {
    stop("all 'trades' columns must have at least one negative trade")
  }

  if(is.null(probs)) {
    probs <- rep(1/NROW(trades),NROW(trades))
  } else {
    if(NROW(trades) != NROW(probs)) {
      stop("'trades' and 'probs' must be same length")
    }
  }

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
  if(nperm < nsamp) {
    for( i in 1:nperm ) {
      perm <- .nPri(NR, horizon, i, replace=TRUE)
      hprPerm <- res[perm]
      probPerm <- prod( probs[perm] )

      cumhpr <- cumprod(c(1,hprPerm))
      cumhpr <- cumhpr/cummax(cumhpr)-(1-DD)
      if( any(cumhpr <= 0) ) {
        failProb <- failProb + probPerm
      }
      sumProb <- sumProb + probPerm
    }

  } else {
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
  }

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

