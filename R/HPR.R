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

HPR <- function(lsp, portfolio=FALSE) {
  if(class(lsp) != "lsp") stop("not a 'lsp' object")
  
  nr <- NROW(lsp$events)
  order <- as.integer(1:nr-1)
  #if(is.null(order)) {
  #} else {
  #  if(!any(order %in% 1:nr)) {
  #    stop("elements in 'order' > lsp events")
  #  }
  #  order <- order - 1
  #}

  hpr <- .Call("hpr", lsp, portfolio, order, PACKAGE="LSPM")

  return(hpr)
}
