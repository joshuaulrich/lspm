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

riskRuin <- function(lsp, DD, horizon, error=0.001, sigma=3, snow=NULL) {

  trades <- lsp$events
  probs <- lsp$probs
  maxLoss <- lsp$maxLoss
  f <- lsp$f
  
  # Portfolio HPR
  hprPort <- HPR(lsp, portfolio=TRUE)

  NR <- NROW(trades)
  nsamp <- ( (sigma/error) ^ 2 ) * 0.25
  nperm <- NR ^ horizon
  
  failProb <- sumProb <- 0

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
    res <- .riskRD(c(1,nperm), DD, horizon, hprPort, probs, ruin=TRUE, sample)
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
    ca <- clusterApply(snow, ind, fun=.riskRD, DD=DD, horizon=horizon,
                       hpr=hprPort, probs=probs, ruin=TRUE, sample)
    
    # sum the fail and total probabilities from all the cores
    seqlen <- 1:length(ca)
    failProb <- sum( sapply(seqlen, function(i) ca[[i]][1]) )
    sumProb  <- sum( sapply(seqlen, function(i) ca[[i]][2]) )
  }

  out <- failProb / sumProb

  return(out)
}

riskDrawdown <- function(lsp, DD, horizon, error=0.001, sigma=3, snow=NULL) {

  trades <- lsp$events
  probs <- lsp$probs
  maxLoss <- lsp$maxLoss
  f <- lsp$f
  
  # Portfolio HPR
  hprPort <- HPR(lsp, portfolio=TRUE)

  NR <- NROW(trades)
  nsamp <- ( (sigma/error) ^ 2 ) * 0.25
  nperm <- NR ^ horizon
  
  failProb <- sumProb <- 0
  
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
    res <- .riskRD(c(1,nperm), DD, horizon, hprPort, probs, ruin=FALSE, sample)
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
    ca <- clusterApply(snow, ind, fun=.riskRD, DD=DD, horizon=horizon,
                       hpr=hprPort, probs=probs, ruin=FALSE, sample)
    
    # sum the fail and total probabilities from all the cores
    seqlen <- 1:length(ca)
    failProb <- sum( sapply(seqlen, function(i) ca[[i]][1]) )
    sumProb  <- sum( sapply(seqlen, function(i) ca[[i]][2]) )
  }

  # calculate the final probability
  out <- failProb / sumProb

  return(out)
}

.riskRD <- function(range, DD, horizon, hpr, probs, ruin, sample) {
  # This is a simple function that can be sent to the cluster
  beg <- range[1]
  end <- range[2]
  res <- .Call('riskRD', beg, end, DD, horizon, hpr, probs,
                         ruin, sample, PACKAGE="LSPM")
  return(res)
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

