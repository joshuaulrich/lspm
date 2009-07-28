lsp.portfolio <- function(lsp.components) {
	if (!class(lsp.components)=="list") {
		stop("lsp.components must be a list")
	}
		
	p <- as.data.frame(lsp.components)
	return(p)
}



