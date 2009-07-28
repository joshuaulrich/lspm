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




