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

.findg <- function(f,jpt,RRDR) {
	g <- 0
	
	worstcase <- vector()
	for(i in 2:ncol(jpt)) {
		worstcase[i-1] <- min(jpt[,i])
	}
	
	hpr <- 1	
	for(i in 1:nrow(jpt)) {
		hprk <- sum(-jpt[i,-1]*f/worstcase)
		if(hprk < -1) {
			hprk <- 0
		} else {
			hprk <- (1 + hprk)^jpt[i,1]
		}
		
		hpr <- hpr*hprk
	}
	
	if(hpr <= 0) {
		g <- 0
	} else {
		g <- hpr^sum(jpt[,1])
	}
	
	return(g)
}




