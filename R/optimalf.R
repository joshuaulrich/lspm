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

optimalf <- function(lsp, constrFun=NULL, constrVal=NULL, ...) {

  # Author: Joshua Ulrich

  # z-values are not needed, so set them to zero so they aren't used
  lsp$z <- rep(0,3)

  fun <- function(f, lsp, constrFun, constrVal, ...) {
    lsp$f <- f
    G <- GHPR(lsp)
    if(G > 1) {
      cons <- 0
      # The intent here is to allow any constraint function / values
      # to be passed to the optimizer.
      # The portion of the list after '...' should be contained in the
      # soon-to-be-created lsp class.
      if(!is.null(constrFun)) {
        cons <- do.call(constrFun, list(lsp, ...))
        
        if(cons >= constrVal) {
          return(Inf)
        } else {
          return(-G)
        }
      } else {
        return(-G)
      }
    } else {
      return(Inf)
    }

  }

  l <- rep(0,NCOL(lsp$events))
  u <- rep(1,NCOL(lsp$events))
  de <- deoptim(fun, lower=l, upper=u, lsp=lsp,
                constrFun=constrFun, constrVal=constrVal,...)

  res <- list(f=de$optim$bestmem, G=-de$optim$bestval)
  names(res$f) <- colnames(lsp$events)
  return(res)
}
