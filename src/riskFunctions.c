/*
#
#   LSPM: The Leverage Space Portfolio Modeler
#
#   Copyright (C) 2009-2010  Soren MacBeth, Joshua Ulrich, and Ralph Vince
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
#include <R_ext/Utils.h>
#include "lspm.h"

SEXP probRD ( SEXP beg, SEXP end, SEXP DD, SEXP lsp,
        SEXP horizon, SEXP sample, SEXP ruin )
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

  int P=0;  /* PROTECT counter */
  int i, j;  /* loop counters */

  /* extract lsp components */
  SEXP event = VECTOR_ELT(lsp, 0);
  SEXP prob = VECTOR_ELT(lsp, 1);
  SEXP fval = VECTOR_ELT(lsp, 2);
  SEXP maxloss = VECTOR_ELT(lsp, 3);
  SEXP zval = VECTOR_ELT(lsp, 4);

  /* ensure lsp components are double */
  if(TYPEOF(event) != REALSXP) {
    PROTECT(event = coerceVector(event, REALSXP)); P++;
  }
  if(TYPEOF(prob) != REALSXP) {
    PROTECT(prob = coerceVector(prob, REALSXP)); P++;
  }
  if(TYPEOF(fval) != REALSXP) {
    PROTECT(fval = coerceVector(fval, REALSXP)); P++;
  }
  if(TYPEOF(maxloss) != REALSXP) {
    PROTECT(maxloss = coerceVector(maxloss, REALSXP)); P++;
  }
  if(TYPEOF(zval) != REALSXP) {
    PROTECT(zval = coerceVector(zval, REALSXP)); P++;
  }

  /* pointers to lsp components */
  double *d_event = REAL(event);
  double *d_prob = REAL(prob);
  double *d_fval = REAL(fval);
  double *d_maxloss = REAL(maxloss);
  double *d_zval = REAL(zval);

  /* To allow for larger nPr, ensure 'beg', 'end', and 'sample' are double */
  if(TYPEOF(beg) != REALSXP) {
    PROTECT(beg = coerceVector(beg, REALSXP)); P++;
  }
  if(TYPEOF(end) != REALSXP) {
    PROTECT(end = coerceVector(end, REALSXP)); P++;
  }
  if(TYPEOF(sample) != REALSXP) {
    PROTECT(sample = coerceVector(sample, REALSXP)); P++;
  }
  /* ensure 'horizon' is integer */
  if(TYPEOF(horizon) != INTSXP) {
    PROTECT(horizon = coerceVector(horizon, INTSXP)); P++;
  }
  /* ensure 'ruin' is logical */
  if(TYPEOF(ruin) != LGLSXP) {
    PROTECT(ruin = coerceVector(ruin, LGLSXP)); P++;
  }
  
  /* Get values from pointers */
  double i_beg = REAL(beg)[0]-1;  /* Convert from one- to zero-based index */
  double i_end = REAL(end)[0]-1;  /* Convert from one- to zero-based index */
  double i_sample = REAL(sample)[0];
  double d_dd = REAL(DD)[0];
  int i_horizon = INTEGER(horizon)[0];
  int i_ruin = INTEGER(ruin)[0];
 
  /* initialize result object and pointer */
  SEXP result;
  PROTECT(result = allocVector(REALSXP, 2)); P++;
  double *d_result = REAL(result);

  /* initialize portfolio HPR object */
  SEXP phpr;
  PROTECT(phpr = allocVector(REALSXP, i_horizon)); P++;

  double I; int J;
  double nr = nrows(prob);
  double failProb = 0;
  double sumProb = 0;
  double *d_phpr = NULL;

  /* does the lsp object have non-zero z values? */
  int using_z = (d_zval[0]==0 && d_zval[1]==0) ? 0 : 1;

  /* initialize object to hold permutation locations if using_z,
   * perm will have 'i_horizon' elements, else 'perm' will have
   * 'nr' elements */
  SEXP perm;
  PROTECT(perm = allocVector(INTSXP, using_z ? i_horizon : nr)); P++;
  int *i_perm = INTEGER(perm);

  /* if lsp object contains z-values of zero, calculate HPR before
   * running permutations */
  if( !using_z ) {
    /* in this case order does not matter, so calculate portfolio
     * HPRs in whatever order the are in the lsp object */
    for(j=0; j<nr; j++) i_perm[j] = j;
    /* call lspm::hpr and assign pointer */
    phpr = hpr(lsp, ScalarLogical(TRUE), perm);
    d_phpr = REAL(phpr);
  }

  /* Initialize R's random number generator (read in .Random.seed) */
  GetRNGstate();

  /* Loop over each permutation index */
  for(i=i_beg; i<=i_end; i++) {

    /* check for user-requested interrupt */
    R_CheckUserInterrupt();

    double probPerm = 1;  /* proability of this permutation */
    double t0hpr = 1;     /* this period's (t = 0) HPR */
    double t1hpr = 1;     /* last period's (t = 1) HPR */
    int fail = 0;         /* fail=1 if ruin or max drawdown is hit */
    
    /* if sampling, get a random permutation between 0 and nPr-1,
     * else use the current index value. */
    I = (i_sample > 0) ? ( unif_rand() * (i_sample-1) ) : i;

    /* set the permutation locations for index 'I' */
    for(j=0; j<i_horizon; j++) {
      i_perm[j] = (long)fmod(I/pow(nr,j),nr);
    }
    /* if lsp object contains non-zero z values, calculate HPR for
     * each permutation */
    if( using_z ) {
      /* call lspm::hpr and assign pointer */
      phpr = hpr(lsp, ScalarLogical(TRUE), perm);
      d_phpr = REAL(phpr);
    }

    /* loop over permutation locations */
    for(j=0; j<i_horizon; j++) {
      /* if using_z, phpr has 'i_horizon' elements, else it has
       * 'nr' elements */
      J = using_z ? j : i_perm[j];
      t1hpr *= d_phpr[J];  /* New portfolio balance */
      /* if ruin % or max drawdown is hit */
      if( t1hpr <= (1-d_dd) ) {
        fail = 1;
      }
      /* If calculating risk drawdown and last period was a new high */
      if( !i_ruin && t1hpr > 1 ) {
        t1hpr = 1;
      }
      /* Keep track of this permutation's probability */
      probPerm *= d_prob[J];
    }
    /* If this permutation hit ruin/drawdown limit,
     * add its probability to the total. */
    if( fail ) {
      failProb += probPerm;
    }
    /* Total probability of all permutations */
    sumProb += probPerm;
  }
  PutRNGstate();  /* Write out .Random.seed */

  /* Store results */
  d_result[0] = failProb;
  d_result[1] = sumProb;
  
  UNPROTECT(P);
  return result;
}

