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

lsp <- function(events, probs=NULL, f=NULL, maxLoss=NULL, z=NULL) {

  events <- as.matrix(events)
  
  if(is.null(probs)) {
    probs <- rep(1/NROW(events),NROW(events))
  }
  if(NROW(events) != NROW(probs)) {
    stop("'events' and 'probs' must be same length")
  }

  if(is.null(f)) {
    f <- rep(0.1, NCOL(events))
  }
  if(NCOL(events) != length(f)) {
    stop("length(f) must equal ncol(events)")
  }

  if(is.null(maxLoss)) {
    maxLoss <- sapply(1:NCOL(events), function(i) min(events[,i]))
  }
  if(NCOL(events) != length(maxLoss)) {
    stop("length(maxLoss) must equal ncol(events)")
  }
  if(any(maxLoss >= 0)) {
    stop("all 'events' columns must have at least one negative trade")
  }
  if(is.null(z)) {
    z <- rep(0L,3)
  }

  x <- structure( list(events = as.matrix(events),
                       probs = as.matrix(probs),
                       f = as.matrix(f),
                       maxLoss = as.matrix(maxLoss),
                       z = as.matrix(z)), class="lsp" )
  return(x)
}

print.lsp <- function(x, ...) {
  y <- cbind(x$probs, x$events)
  cxe <- colnames(x$events)
  if(is.null(cxe)) cxe <- paste("V",1:NCOL(x$events),sep="")
  colnames(y) <- c("probs",cxe)
  
  #atr <- rbind(x$f, x$maxLoss)
  atr <- t(cbind(x$f,x$maxLoss))
  rownames(atr) <- c("f","Max Loss")
  colnames(atr) <- cxe
  
  print(atr)
  print(y)
}
