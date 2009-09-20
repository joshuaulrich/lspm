lsp <- function(events, probs=NULL, f=NULL, maxLoss=NULL) {

  events <- as.matrix(events)
  
  if(is.null(probs)) {
    probs <- rep(1/NROW(events),NROW(events))
  }
  if(NROW(events) != NROW(probs)) {
    stop("'events' and 'probs' must be same length")
  }

  if(is.null(f)) {
    f <- rep(0.1, NCOL(events))
  }
  if(NCOL(events) != length(f)) {
    stop("length(f) must equal ncol(events)")
  }

  if(is.null(maxLoss)) {
    maxLoss <- sapply(1:NCOL(events), function(i) min(events[,i]))
  }
  if(NCOL(events) != length(maxLoss)) {
    stop("length(maxLoss) must equal ncol(events)")
  }
  if(any(maxLoss >= 0)) {
    stop("all 'events' columns must have at least one negative trade")
  }

  x <- structure( list(events = as.matrix(events),
                       probs = as.matrix(probs),
                       f = as.matrix(f),
                       maxLoss = as.matrix(maxLoss)), class="lsp" )
  return(x)
}

print.lsp <- function(x, ...) {
  y <- cbind(x$probs, x$events)
  cxe <- colnames(x$events)
  if(is.null(cxe)) cxe <- paste("V",1:NCOL(x$events),sep="")
  colnames(y) <- c("probs",cxe)
  
  #atr <- rbind(x$f, x$maxLoss)
  atr <- t(cbind(x$f,x$maxLoss))
  rownames(atr) <- c("f","Max Loss")
  colnames(atr) <- cxe
  
  print(atr)
  print(y)
}
