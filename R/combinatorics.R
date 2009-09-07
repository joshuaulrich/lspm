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

".combinadic" <- function(n, r, i) {

  # http://msdn.microsoft.com/en-us/library/aa289166(VS.71).aspx
  # http://en.wikipedia.org/wiki/Combinadic

  if(i < 0 | i > choose(n,r)) stop("'i' must be 0 < i <= n!/(n-r)!")
  
  largestV <- function(n, r, i) {
    #v <- n-1
    v <- n                                  # Adjusted for one-based indexing
    #while(choose(v,r) > i) v <- v-1
    while(choose(v,r) >= i) v <- v-1        # Adjusted for one-based indexing
    return(v)
  }

  res <- rep(NA,r)
  for(j in 1:r) {
    res[j] <- largestV(n,r,i)
    i <- i-choose(res[j],r)
    n <- res[j]
    r <- r-1
  }
  res <- res + 1
  return(res)
}

".factoradic" <- function(n, i) {

  # http://msdn.microsoft.com/en-us/library/aa302371.aspx
  # http://en.wikipedia.org/wiki/Factoradic

  # Check / adjust for one-based indexing
  if(i < 1) stop("'i' must be > 0")
  i <- as.integer(i - 1)

  # Initialize result vector
  res <- rep(NA,n)

  # Calculate factoradic of 'i'
  for(j in 1:n) {
    res[n-j+1] <- i %% j + 1
    i <- as.integer(i/j)
  }

  return(res)
}

".nPriR" <- function(n, r, i, replace=FALSE) {
  
  # http://msdn.microsoft.com/en-us/library/aa302371.aspx
  # http://en.wikipedia.org/wiki/Factoradic

  if(replace) {
    N <- n^r
  } else {
    N <- factorial(n)/factorial(n-r)
  }
  if(i > N) stop('index larger than number of permutations')
  
  if(replace) {
    
    # Initialize result vector
    res <- rep(NA,r)

    for( j in 1:r ) {
      res[r-j+1] <- as.integer((i-1)/as.integer(n^(j-1))) %% n + 1
    }

  } else {
    # MSDN method

    # Initialize result vector
    res <- rep(NA,n)
    
    # If r < (n-1), we only want the first r elements of the k*(n-r)!
    # permutation (by Joshua Ulrich, not part of the MSDN algorithm).
    if( r < (n-1) ) {
      i <- i * prod(1:(n-r))
    }

    # Step #1 - Find factoradic of i
    tmp <- .factoradic(n, i)
    
    # Step #2 - Convert factoradic to permuatation
    # Set right-most element to 1.
    tmp[n] <- res[n] <- 1
    
    # Note what's going on here...
    for( i in (n-1):1 ) {
      res[i] <- tmp[i]
      for( j in (i+1):n ) {
        if( res[j] >= res[i] ) res[j] <- res[j] + 1
      }
    }
  }
  
  # Only return requested elements
  res <- res[1:r]

  return(res)
}

".nCri" <- function(n, r, i) {
  
  # http://msdn.microsoft.com/en-us/library/aa289166(VS.71).aspx
  # http://en.wikipedia.org/wiki/Combinadic

  if(i < 0 | i > choose(n,r)) stop("'i' must be 0 < i <= n!/(n-r)!")

  #dual <- choose(n,r)-1-i
  dual <- choose(n,r)-i+1                     # Adjusted for one-based indexing

  res <- .combinadic(n, r, dual)
  for(j in 1:r) {
    #res[j] <- (n-1) - res[j]
    res[j] <- n - res[j]                    # Adjusted for one-based indexing
  }
  return(res+1)
}

".nPri" <- function(n, r, i, replace=FALSE) {
  
  # http://msdn.microsoft.com/en-us/library/aa302371.aspx
  # http://en.wikipedia.org/wiki/Factoradic

  if(replace) {
    N <- n^r
  } else {
    N <- factorial(n)/factorial(n-r)
  }
  if(i > N) stop('index larger than number of permutations')
  
  res <- .Call('nPri', n, r, i, replace)
  
  # Only return requested elements
  res <- res[1:r]

  return(res)
}

