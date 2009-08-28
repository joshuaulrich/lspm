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

.obj.function <- function(f, jpt, max.probprofit, RRDR, RRDRPP) {	
	msnames <- colnames(jpt)[-1]
	if (length(msnames) == 1) {
		msnames <- .jarray(msnames, "S")
	}
	
	if (length(f) == 1) {
		f.jcall <- .jarray(f, "D")
	} else {
		f.jcall <- f
	}
	
	probs <- jpt[,1]
	plays <- as.matrix(jpt[,-1])
	d <- dim(plays)
	
	if (max.probprofit) {
		passed <- .jcall(RRDRPP,"Z","test",msnames,f.jcall,as.vector(probs),as.vector(t(plays)),as.integer(d))
		if(passed) {
			probprofitathorizon <- .jfield(RRDRPP,"D","probprofitathorizon")
			return(-probprofitathorizon)
		} else {
			return(Inf)
		}
	} else {
		g <- .findg(f,jpt,RRDR)
		if(g > 1) {
			passed <- .jcall(RRDR,"Z","test",msnames,f.jcall,as.vector(probs),as.vector(t(plays)),as.integer(d))
			if(passed) {
				return(-g)
			} else {
				return(Inf)
			}
		} else {
			return(Inf)
		}
	}
}
