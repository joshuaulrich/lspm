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
