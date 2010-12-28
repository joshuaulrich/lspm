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

optimalf <- function(lsp, constrFun=NULL, constrVal=NULL,
  margin=NULL, equity=NULL, upper, lower, ...) {

  # Author: Joshua Ulrich

  # z-values are not needed, so set them to zero so they aren't used
  lsp$z <- rep(0,3)

  # Check margin constraint arguments
  if(!is.null(margin) && !is.null(equity)) {
    if(NROW(margin) != NCOL(lsp$events)) {
      stop(paste("'margin' must have length =",NCOL(lsp$events)))
    }
  } else
  if(!is.null(margin) &&  is.null(equity) ||
      is.null(margin) && !is.null(equity)) {
    stop("both 'equity' and 'margin' must be provided")
  }

  fun <- function(f, lsp, constrFun, constrVal, margin, equity, ...) {
    .Call("objFun_optimalf", f, lsp, margin, equity,
      constrFun, constrVal, new.env(), PACKAGE="LSPM")
  }

  # Allow user-specified bounds
  nc <- NCOL(lsp$events)
  l <- rep(0,nc)
  if(!missing(lower)) {
    l[1:nc] <- lower
  }
  u <- rep(1,nc)
  if(!missing(upper)) {
    u[1:nc] <- upper
  }

  de <- DEoptim(fun, lower=l, upper=u, lsp=lsp,
                constrFun=constrFun, constrVal=constrVal,
                margin=margin, equity=equity, ...)

  res <- list(f=de$optim$bestmem, G=-de$optim$bestval)
  names(res$f) <- colnames(lsp$events)
  return(res)
}
