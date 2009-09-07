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

GHPR <- function(f, trades, probs=NULL, maxLoss=NULL) {

  # Author: Joshua Ulrich
  
  if(length(f) != NCOL(trades)) {
    stop("length(f) must equal ncol(trades)")
  }
  
  if(is.null(maxLoss)) {
    maxLoss <- sapply(1:NCOL(trades), function(i) min(as.matrix(trades)[,i]))
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

  #hpr <- as.matrix(-f * trades / maxLoss)
  hpr <- sapply(1:NCOL(trades), function(i) -f[i]*trades[,i]/maxLoss[i])
  #res <- prod( (1+hpr)^probs ) ^ (1/sum(probs))
  res <- prod( sapply(1:NROW(hpr), function(i) max(0,(1+sum(hpr[i,])))^probs[i]) ) ^ (1/sum(probs))

  return(res)
}
