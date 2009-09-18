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
//#include "lspm.h"

SEXP riskRD ( SEXP DD, SEXP horizon, SEXP hpr, SEXP prob,
        SEXP ruin, SEXP beg, SEXP end )
{
  /* Arguments:
   *   drawdown %
   *   horizon
   *   hprs
   *   probs
   *   ruin/DD boolean
   *   replace boolean
   *   beginning index value
   *   ending index value
   */

  /* Notes:
   *    1) Send start/end index values to each process.
   *    2) replace=TRUE tells you when to sample, and you only sample between
   *       the start/end values.  This should work because we're sampling from
   *       a uniform distribution.
   *    3) Create a drawdown/ruin switch to simplify the code.
   */

  int P=0;
  int i, j;

  SEXP result;
  double *d_result = NULL;
  
  double *d_dd=NULL, *d_hpr=NULL, *d_prob=NULL; 
  d_dd = REAL(DD);
  d_hpr = REAL(hpr);
  d_prob = REAL(prob);
  
  // ensure 'horizon' is integer
  if(TYPEOF(horizon) != INTSXP) {
    PROTECT(horizon = coerceVector(horizon, INTSXP)); P++;
  }
  // ensure 'beg' is integer
  if(TYPEOF(beg) != INTSXP) {
    PROTECT(beg = coerceVector(beg, INTSXP)); P++;
  }
  // ensure 'end' is integer
  if(TYPEOF(end) != INTSXP) {
    PROTECT(end = coerceVector(end, INTSXP)); P++;
  }
  // ensure 'ruin' is logical
  if(TYPEOF(ruin) != LGLSXP) {
    PROTECT(ruin = coerceVector(ruin, LGLSXP)); P++;
  }
  
  int i_beg, i_end, i_horizon;
  i_beg = INTEGER(beg)[0];
  i_end = INTEGER(end)[0];
  i_horizon = INTEGER(horizon)[0];
 
  // replace = TRUE only currently supported
  //SEXP replace;
  //PROTECT(replace = allocVector(LGLSXP, 1)); P++;
  //LOGICAL(replace)[0] = 1;
  
  PROTECT(result = allocVector(REALSXP, 2)); P++;
  d_result = REAL(result);

  int perm;
  int nr = nrows(hpr);
  double failProb = 0;
  double sumProb = 0;

  for(i=i_beg; i<=i_end; i++) {

    double probPerm = 1;
    double t0hpr = 1;  // this period's (t = 0) HPR
    double t1hpr = 1;  // last period's (t = 1) HPR
    int fail = 0;

    for(j=0; j<i_horizon; j++) {
      
      //if( replace )
      //{
      perm = (long)(i/pow(nr,j)) % nr;
      //} else {
      //}

      t0hpr = d_hpr[perm];
      t1hpr *= t0hpr;
      if( t1hpr <= (1-*d_dd) ) {
        fail = 1;
      }
      // only drawdown only at the moment
      if( t1hpr > 1 ) {
        t1hpr = 1;
      }

      probPerm *= d_prob[perm];
    }
    if( fail ) {
      failProb += probPerm;
    }
    sumProb += probPerm;

  }

  // store results
  d_result[0] = failProb;
  d_result[1] = sumProb;
  
  UNPROTECT(P);
  return result;
}

