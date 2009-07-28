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