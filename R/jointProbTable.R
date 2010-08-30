#
#   LSPM: The Leverage Space Portfolio Modeler
#
#   Copyright (C) 2009-2010  Soren Macbeth, Joshua Ulrich, and Ralph Vince
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

jointProbTable <- function(x, n=3, FUN=median, ...) {

  # Author: Joshua Ulrich & Soren Macbeth

  # Load LSPM
  if(!require(LSPM,quietly=TRUE)) stop(warnings())

  # Function to bin data
  quantize <- function(x, n, FUN=median, ...) {
    if(is.character(FUN)) FUN <- get(FUN)
    bins <- cut(x, n, labels=FALSE)
    res <- sapply(1:NROW(x), function(i) FUN(x[bins==bins[i]], ...))
  }

  # Allow for different values of 'n' for each system in 'x'
  if(NROW(n)==1) {
    n <- rep(n,NCOL(x))
  } else
  if(NROW(n)!=NCOL(x)) stop("invalid 'n'")

  # Bin data in 'x'
  qd <- sapply(1:NCOL(x), function(i) quantize(x[,i],n=n[i],FUN=FUN,...))

  # Aggregate probabilities
  probs <- rep(1/NROW(x),NROW(x))
  res <- aggregate(probs, by=lapply(1:NCOL(qd), function(i) qd[,i]), sum)

  # Clean up output, return lsp object
  colnames(res) <- colnames(x)
  res <- lsp(res[,1:NCOL(x)],res[,NCOL(res)], maxLoss=apply(x,2,min))

  return(res)
}

