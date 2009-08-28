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

lsp.component <- function(x,num.bins) {
	gen.scenarios <- function(x) {
		labs <- levels(x)
		lower <- as.numeric( sub("\\((.+),.*", "\\1", labs))
		upper <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs))
		data.frame(lower=lower,mid=(upper+lower)/2,upper=upper)
	}
	
	#c <- list()
	#c$hprs <- x
	#c$biggestLoser <- min(c$hprs)
	scenarios <- gen.scenarios(cut(x,num.bins))
	c <- as.numeric(as.character(cut(x,num.bins,labels=scenarios$mid)))
	return(c)
}
