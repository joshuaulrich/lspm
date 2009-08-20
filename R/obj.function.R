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
