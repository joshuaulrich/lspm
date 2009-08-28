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
