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

HPR <- function(lsp, portfolio=FALSE) {
  if(class(lsp) != "lsp") stop("not a 'lsp' object")
  NC <- NCOL(lsp$events)
  NR <- NROW(lsp$events)
  
  #hpr <- sapply(1:NC, function(i) 1-lsp$f[i]*lsp$events[,i]/lsp$maxLoss[i])
  hpr <- lapply(1:NC, function(i) 1-lsp$f[i]*lsp$events[,i]/lsp$maxLoss[i])
  hpr <- matrix(unlist(hpr),ncol=NC,nrow=NR)

  if(portfolio) {
    #hpr <- sapply(1:NR, function(i) (1+sum(hpr[i,]-1)))
    hpr <- unlist(lapply(1:NR, function(i) (1+sum(hpr[i,]-1))))
  }
  return(as.matrix(hpr))
}
