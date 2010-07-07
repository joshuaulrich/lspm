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

maxProbProfit <- function(lsp, target, horizon, constrFun=NULL, constrVal=NULL,
  zmin=-1, margin=NULL, equity=NULL, upper, lower, ...) {

  # Author: Joshua Ulrich

  lsp$z[3] <- target

  # Check exponent arguments
  if(length(zmin)==1) {
    zmin <- rep(zmin,2)
  } else {
    zmin <- zmin[1:2]
  }
  if(max(zmin)>0 | min(zmin) < -1) stop("zmin must be -1 <= zmin <= 0")

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

  fun <- function(fz, lsp, horizon, constrFun, constrVal,
    margin, equity, ...) {
    
    ns <- NROW(lsp$f)
    lsp$f <- fz[1:ns]
    lsp$z[1:2] <- fz[(ns+1):(ns+2)]

    # Margin constraint(s)
    if(!(is.null(margin) & is.null(equity))) {
      maxU <- sum( equity * lsp$f * margin / -lsp$maxLoss )
      if(maxU > equity) return(Inf) 
    }

    if(!is.null(constrFun)) {
      # This is in case the constraint function has a 'horizon' arg.
      # Since 'horizon' is a formal arg of maxProbProfit, it matches
      # and is no longer in '...' for use by constrFun
      cDots <- list(...)
      cFnNames <- names(formals(constrFun))
      cDots <- cDots[names(cDots) %in% cFnNames]
      if("horizon" %in% cFnNames) {
        cDots$horizon <- horizon
      }
      cons <- do.call(constrFun, c(list(lsp), cDots))
        
      if(cons >= constrVal) {
        return(Inf)
      }
    }
    # This allows args like 'error', 'sigma', and 'snow' to be
    # passed to probProfit, while removing any args that don't
    # belong to probProfit.
    pFnNames <- names(formals(probProfit))
    pDots <- list(...)
    pDots <- pDots[names(pDots) %in% pFnNames]
    P <- do.call(probProfit, c(list(lsp, lsp$z[3], horizon), pDots))
    return(-P)
  }

  # Allow user-specified bounds
  nc <- NCOL(lsp$events)
  l <- rep(0,nc)
  if(!missing(lower)) {
    l[1:nc] <- lower
  }
  l <- c(l,zmin[1],zmin[2])
  u <- rep(1,nc)
  if(!missing(upper)) {
    u[1:nc] <- upper
  }
  u <- c(u,0,0)

  de <- deoptim(fun, lower=l, upper=u, lsp=lsp, horizon=horizon,
                constrFun=constrFun, constrVal=constrVal,
                margin=margin, equity=equity, ...)

  ns <- NROW(de$optim$bestmem)
  res <- list(f=de$optim$bestmem[1:(ns-2)], z=de$optim$bestmem[(ns-1):ns],
              profitProb=-de$optim$bestval)
  names(res$f) <- colnames(lsp$events)
  names(res$z) <- c("zminus","zplus")
  return(res)
}
