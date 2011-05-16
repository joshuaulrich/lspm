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

maxDrawdown <- function(lsp, sort=FALSE, geometric=TRUE) {
 # lsp: lsp-class object
 # sort: produce worst possible DD by sorting portfolio HPRs?
 # geometric: continuous or discrete DD calculation?
 hprp <- HPR(lsp,TRUE)
 if(sort) {
   maxDD <- max(1-cumprod(hprp[hprp < 1]))
 } else {
   hprp <- c(1,hprp)
   if(geometric) {
     maxDD <- max(-log(cumprod(hprp)/cummax(cumprod(hprp))))
   } else {
     maxDD <- max( 1 - cumprod(hprp)/cummax(cumprod(hprp)))
   }
 }
 maxDD
}

