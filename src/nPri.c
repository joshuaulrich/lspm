/*
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
*/

#include <R.h>
#include <Rinternals.h>

SEXP nPri ( SEXP n, SEXP r, SEXP i, SEXP replace )
{
  int P=0;
  int j, k;
  int int_n, int_r, int_i, int_replace;
  
  SEXP result;
  int *int_result=NULL;

  // ensure 'n' is integer
  if(TYPEOF(n) != INTSXP) {
    PROTECT(n = coerceVector(n, INTSXP)); P++;
  }
  // ensure 'r' is integer
  if(TYPEOF(r) != INTSXP) {
    PROTECT(r = coerceVector(r, INTSXP)); P++;
  }
  // ensure 'i' is integer
  if(TYPEOF(i) != INTSXP) {
    PROTECT(i = coerceVector(i, INTSXP)); P++;
  }
  // ensure 'replace' is logical
  if(TYPEOF(replace) != LGLSXP) {
    PROTECT(replace = coerceVector(replace, LGLSXP)); P++;
  }

  // get the first element (everything from R is a vector)
  int_n = INTEGER(n)[0];
  int_r = INTEGER(r)[0];
  int_i = INTEGER(i)[0];
  int_replace = INTEGER(replace)[0];

  if ( int_replace ) {
    // Permutations WITH replacement
    PROTECT(result = allocVector(INTSXP, int_r)); P++;
    int_result = INTEGER(result);

    for (j=0; j < int_r; ++j) {
      int_result[int_r-j-1] = ( (long)(int_i/pow(int_n,j)) % int_n );
    }
  } else {
    // Permutations WITHOUT replacement
    PROTECT(result = allocVector(INTSXP, int_n)); P++;
    int_result = INTEGER(result);

    // Adjust i for r < (n-1)
    //   If r < (n-1) then we want the first r elements of the i*(n-r)!
    //   permutation (by Joshua Ulrich, not part of the MSDN algorithm).
    if ( int_r < (int_n-1) ) {
      // Compute factorial multiplier
      int fmult = 1;
      for (j = 1; j <= (int_n-int_r); ++j) {
        fmult = fmult * j;
      }
      // Apply factorial multiplier
      int_i = int_i * fmult;
    }
    
    // Algorithm taken from:
    // http://msdn.microsoft.com/en-us/library/aa302371.aspx
    
    // Step #1 - Find factoradic of i
    int factoradic[int_n];
    for (j = 1; j <= int_n; ++j) {
      factoradic[int_n-j] = int_i % j;
      int_i /= j;
    }
    // Step #2 - Convert factoradic to permuatation
    for (j = 0; j < int_n; ++j) {
      ++factoradic[j];
    }
    // Set right-most element to 1.
    int_result[int_n-1] = 1;  
    // Note what's going on here...
    for (j = int_n-2; j >= 0; --j) {
      int_result[j] = factoradic[j];
      for (k = j+1; k < int_n; ++k) {
        if (int_result[k] >= int_result[j])
          ++int_result[k];
      }
    }
    // Put in zero-based form
    for (j = 0; j < int_n; ++j) {
      --int_result[j];
    }

  }

  UNPROTECT(P);
  return result;
}
