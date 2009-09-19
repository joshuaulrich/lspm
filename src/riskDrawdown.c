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

SEXP riskRD ( SEXP beg, SEXP end, SEXP DD, SEXP horizon,
        SEXP hpr, SEXP prob, SEXP ruin, SEXP sample )
{
  /* Arguments:
   *   beg      First permutation index value
   *   end      Last permutation index value
   *   DD       If ruin=TRUE:   % where ruin occurs
   *            If ruin=FALSE:  maximum drawdown
   *   horizon  Horizon over which to determine risk
   *   hpr      Holding period returns
   *   prob     Probability of each HPR
   *   ruin     ruin/drawdown boolean
   *   sample   If sample=0, run all permutations
   *            else run 'end - beg' random permutations
   *   replace  boolean (not implemented, always replace)
   */

  int P=0;
  int i, j;

  double *d_dd=NULL, *d_hpr=NULL, *d_prob=NULL; 
  d_dd = REAL(DD);
  d_hpr = REAL(hpr);
  d_prob = REAL(prob);
  
  // To allow for larger nPr, ensure 'beg', 'end', and 'sample' are double
  if(TYPEOF(beg) != REALSXP) {
    PROTECT(beg = coerceVector(beg, REALSXP)); P++;
  }
  if(TYPEOF(end) != REALSXP) {
    PROTECT(end = coerceVector(end, REALSXP)); P++;
  }
  if(TYPEOF(sample) != REALSXP) {
    PROTECT(sample = coerceVector(sample, REALSXP)); P++;
  }
  // ensure 'horizon' is integer
  if(TYPEOF(horizon) != INTSXP) {
    PROTECT(horizon = coerceVector(horizon, INTSXP)); P++;
  }
  // ensure 'ruin' is logical
  if(TYPEOF(ruin) != LGLSXP) {
    PROTECT(ruin = coerceVector(ruin, LGLSXP)); P++;
  }
  
  // Get values from pointers
  double i_beg = REAL(beg)[0]-1;  // Convert from one- to zero-based index
  double i_end = REAL(end)[0]-1;  // Convert from one- to zero-based index
  double i_sample = REAL(sample)[0];
  int i_horizon = INTEGER(horizon)[0];
  int i_ruin = INTEGER(ruin)[0];
 
  // Initialize 'result' vector and pointer
  SEXP result;
  PROTECT(result = allocVector(REALSXP, 2)); P++;
  double *d_result = NULL;
  d_result = REAL(result);

  long perm;
  double I;
  double nr = nrows(hpr);
  double failProb = 0;
  double sumProb = 0;

  // Initialize R's random number generator (read in .Random.seed)
  GetRNGstate();  

  // Loop over each permutation index
  for(i=i_beg; i<=i_end; i++) {

    double probPerm = 1;  // proability of this permutation
    double t0hpr = 1;     // this period's (t = 0) HPR
    double t1hpr = 1;     // last period's (t = 1) HPR
    int fail = 0;         // fail=1 if ruin or max drawdown is hit
    
    // Get a random permutation between 0 and nPr-1
    if( i_sample > 0 ) {
      I = unif_rand() * (i_sample-1);
    }

    // Loop over permutation
    for(j=0; j<i_horizon; j++) {
      
      // nPri should replace the code below, once
      // I get it working with arrays/pointers...
      //if( replace )
      //{
        if( i_sample > 0 ) {
          //perm = (long)(I/pow(nr,j)) % nr;
          perm = (long)fmod(I/pow(nr,j), nr);
        } else {
          //perm = (long)(i/pow(nr,j)) % nr;
          perm = (long)fmod(i/pow(nr,j), nr);
        }
      //} else {
      //}
      t0hpr = d_hpr[perm];  // Get this period's HPR
      t1hpr *= t0hpr;       // New portfolio balance
      // if ruin % or max drawdown is hit
      if( t1hpr <= (1-*d_dd) ) {
        fail = 1;
      }
      // If calculating risk drawdown and last period was a new high
      if( !i_ruin && t1hpr > 1 ) {
        t1hpr = 1;
      }
      // Keep track of this permutation's probability
      probPerm *= d_prob[perm];
    }
    // If this permutation hit ruin/drawdown limit,
    // add its probability to the total.
    if( fail ) {
      failProb += probPerm;
    }
    // Total probability of all permutations
    sumProb += probPerm;
  }
  PutRNGstate();  // Write out .Random.seed

  // Store results
  d_result[0] = failProb;
  d_result[1] = sumProb;
  
  UNPROTECT(P);
  return result;
}

