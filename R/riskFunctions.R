#
#   LSPM: The Leverage Space Portfolio Modeler
#
#   Copyright (C) 2009-2010  Soren MacBeth, Joshua Ulrich, and Ralph Vince
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

probRuin <-
function(lsp, DD, horizon, calc.max=10, error=0.001, sigma=3, snow=NULL) {

  if( horizon > calc.max ) {
    rd <- sapply( 1:calc.max, function(i) probRuin(lsp, DD,
      i, calc.max, error=error, sigma=sigma, snow=snow) )
    out <- RD.asymptote(rd, horizon, ruin=TRUE)
    return(out)
  }
  
  NR <- NROW(lsp$probs)
  nsamp <- ( (sigma/error) ^ 2 ) * 0.25
  nperm <- NR ^ horizon
  
  sample <- 0  # sampling off by default
  if(nperm > nsamp) {
    # this turns sampling on and tells the function to sample
    # permutations from 1:nperm
    sample <- nperm
    # this lowers the number of permutations to be calculated
    # to nsamp
    nperm <- nsamp
  }

  if(is.null(snow)) {
    
    # calculate all the permutations on the local machine
    res <- .probRD(c(1,nperm), DD, lsp, horizon, sample, ruin=TRUE)
    failProb <- res[1]
    sumProb  <- res[2]
  } else {

    # number of cores in snow cluster
    ncores <- length(snow)

    # divide all the needed calculations evenly among all the cores.
    ij <- c(seq(1, nperm, nperm/ncores),
            seq(nperm/ncores, nperm, nperm/ncores))
    ij <- sort(ij)
    ind <- list()
    for(i in 1:ncores) ind <- c(ind, list(ij[(i-1)*2+(1:2)]))

    # send the function to the cluster
    ca <- clusterApply(snow, ind, fun=.probRD, DD=DD, lsp=lsp,
                       horizon=horizon, sample, ruin=TRUE)
    
    # sum the fail and total probabilities from all the cores
    seqlen <- 1:length(ca)
    failProb <- sum( sapply(seqlen, function(i) ca[[i]][1]) )
    sumProb  <- sum( sapply(seqlen, function(i) ca[[i]][2]) )
  }

  out <- failProb / sumProb

  return(out)
}

probDrawdown <-
function(lsp, DD, horizon, calc.max=10, error=0.001, sigma=3, snow=NULL) {

  if( horizon > calc.max ) {
    rd <- sapply( 1:calc.max, function(i) probDrawdown(lsp, DD,
      i, calc.max, error, sigma, snow) )
    out <- RD.asymptote(rd, horizon, ruin=FALSE)
    return(out)
  }
  
  NR <- NROW(lsp$probs)
  nsamp <- ( (sigma/error) ^ 2 ) * 0.25
  nperm <- NR ^ horizon
  
  sample <- 0  # sampling off by default
  if(nperm > nsamp) {
    # this turns sampling on and tells the function to sample
    # permutations from 1:nperm
    sample <- nperm
    # this lowers the number of permutations to be calculated
    # to nsamp
    nperm <- nsamp
  }

  if(is.null(snow)) {
    
    # calculate all the permutations on the local machine
    res <- .probRD(c(1,nperm), DD, lsp, horizon, sample, ruin=FALSE)
    failProb <- res[1]
    sumProb  <- res[2]
  } else {

    # number of cores in snow cluster
    ncores <- length(snow)

    # divide all the needed calculations evenly among all the cores.
    ij <- c(seq(1, nperm, nperm/ncores),
            seq(nperm/ncores, nperm, nperm/ncores))
    ij <- sort(ij)
    ind <- list()
    for(i in 1:ncores) ind <- c(ind, list(ij[(i-1)*2+(1:2)]))

    # send the function to the cluster
    ca <- clusterApply(snow, ind, fun=.probRD, DD=DD, lsp=lsp,
                       horizon=horizon, sample, ruin=FALSE)
    
    # sum the fail and total probabilities from all the cores
    seqlen <- 1:length(ca)
    failProb <- sum( sapply(seqlen, function(i) ca[[i]][1]) )
    sumProb  <- sum( sapply(seqlen, function(i) ca[[i]][2]) )
  }

  # calculate the final probability
  out <- failProb / sumProb

  return(out)
}

.probRD <- function(range, DD, lsp, horizon, sample, ruin) {
  # This is a simple function that can be sent to the cluster
  beg <- range[1]
  end <- range[2]
  res <- .Call('probRD', beg, end, DD, lsp, horizon,
                         sample, ruin, PACKAGE="LSPM")
  return(res)
}
  
RD.asymptote <- function(RD, horizon, ruin=FALSE) {

  if(ruin) {
    lower <- c(0,0,0)
    upper <- c(1,1,1)
    NP <- 30
    objFun <- function(params, RD, horizons) {
      res <- params[1] - params[2] * exp( -params[3] * horizons )
      res <- sum( (RD - res)^2 )
      return(res)
    }
  } else {
    lower <- c(0,0)
    upper <- c(1,1)
    NP <- 20
    objFun <- function(params, RD, horizons) {
      res <- 1 - params[1] * exp( -params[2] * horizons )
      res <- sum( (RD - res)^2 )
      return(res)
    }
  }

  de <- deoptim(objFun, lower=lower, upper=upper,
    control=list(NP=NP, refresh=-1, strategy=3, noimprove=10, CR=0.9),
    RD=RD, horizons=1:NROW(RD))

  de.coef <- de$optim$bestmem

  if(ruin) {
    out <- de.coef[1] - de.coef[2] * exp( -de.coef[3] * horizon )
  } else {
    out <- 1 - de.coef[1] * exp( -de.coef[2] * horizon )
  }

  names(out) <- NULL
  return(out)
}

