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

lsp.jptable <- function(lsp.portfolio) {	
	order.lsp.portfolio <- do.call(order,as.data.frame(lsp.portfolio))
	equal.to.previous <- rowSums(lsp.portfolio[tail(order.lsp.portfolio,-1),] != lsp.portfolio[head(order.lsp.portfolio,-1),])==0
	tf.runs <- rle(equal.to.previous)
	probability <- c(1, unlist(mapply( function(x,y) if (y) x+1 else (rep(1,x)), tf.runs$length, tf.runs$value )))
	probability <- probability[ c(diff(probability) <= 0, TRUE ) ]
	probability <- probability/nrow(lsp.portfolio)
	unique.rows <- which( c(TRUE, !equal.to.previous ) )
	jpt <- cbind( probability, lsp.portfolio[order.lsp.portfolio[ unique.rows ], ,drop=F] )
	rownames(jpt) <- NULL
	return(as.matrix(jpt))
}
