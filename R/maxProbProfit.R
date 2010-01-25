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

maxProbProfit <- function(lsp, target, horizon,
  constrFun=NULL, constrVal=NULL, ...) {

  # Author: Joshua Ulrich

  lsp$z[3] <- target
  
  fun <- function(fz, lsp, horizon, constrFun, constrVal, ...) {
    
    ns <- NROW(lsp$f)
    lsp$f <- fz[1:ns]
    lsp$z[1:2] <- fz[(ns+1):(ns+2)]

    if(!is.null(constrFun)) {
      cons <- do.call(constrFun, list(lsp, ...))
        
      if(cons >= constrVal) {
        return(Inf)
      }
    }
    P <- probProfit(lsp, lsp$z[3], horizon, ...)
    return(-P)
  }

  l <- c(rep(0,NCOL(lsp$events)),-1,-1)
  u <- c(rep(1,NCOL(lsp$events)), 0, 0)
  de <- deoptim(fun, lower=l, upper=u, lsp=lsp, horizon=horizon,
                constrFun=constrFun, constrVal=constrVal, ...)

  ns <- NROW(de$optim$bestmem)
  res <- list(f=de$optim$bestmem[1:(ns-2)], z=de$optim$bestmem[(ns-1):ns],
              profitProb=-de$optim$bestval)
  names(res$f) <- colnames(lsp$events)
  return(res)
}
