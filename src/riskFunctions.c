/*
#
#   LSPM: The Leverage Space Portfolio Modeler
#
#   Copyright (C) 2009-2010  Soren Macbeth, Joshua Ulrich, and Ralph Vince
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
#include <Rdefines.h>
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
  int i, j, k;  /* loop counters */

  /* extract lsp components */
  //double *d_event   = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 0)))); P++;
  double *d_prob    = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 1)))); P++;
  //double *d_fval    = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 2)))); P++;
  //double *d_maxloss = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 3)))); P++;
  double *d_zval    = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 4)))); P++;

  /* Get values from pointers */
  double i_beg = asReal(beg)-1;  /* zero-based */
  double i_end = asReal(end)-1;  /* zero-based */
  double i_sample = asReal(sample);
  double d_dd = 1-asReal(DD);
  int i_horizon = asInteger(horizon);
  int i_ruin = asInteger(ruin);

  /* initialize result objects and pointers*/
  SEXP result, outFail, outSum;
  PROTECT(outFail = allocVector(REALSXP, i_horizon)); P++;
  PROTECT(outSum  = allocVector(REALSXP, i_horizon)); P++;
  double *d_outFail = REAL(outFail);
  double *d_outSum  = REAL(outSum);
  for(j=0; j<i_horizon; j++) {
    d_outFail[j] = 0.0;
    d_outSum[j] = 0.0;
  }

  /* initialize portfolio HPR object */
  SEXP phpr;

  double I;
  double nr = nrows(VECTOR_ELT(lsp, 1));
  double *d_phpr = NULL;

  /* does the lsp object have non-zero z values? */
  int using_z = (d_zval[0]==0 && d_zval[1]==0) ? 0 : 1;

  /* initialize object to hold permutation locations */
  SEXP perm;
  PROTECT(perm = allocVector(INTSXP, i_horizon)); P++;
  int *i_perm = INTEGER(perm);

  /* if lsp object contains z-values of zero, calculate HPR before
   * running permutations */
  if( !using_z ) {
    PROTECT(phpr = hpr(lsp, ScalarLogical(TRUE), R_NilValue)); P++;
    d_phpr = REAL(phpr);
  }

  /* Initialize R's random number generator (read in .Random.seed) */
  if(i_sample > 0) GetRNGstate();

  /* proabilities for this permutation */
  double probPerm[i_horizon];
  double t1hpr;     /* last period's (t = 1) HPR */
    
  /* Loop over each permutation index */
  for(i=i_beg; i<=i_end; i++) {

    /* check for user-requested interrupt */
    if( i % 10000 == 999 ) R_CheckUserInterrupt();

    t1hpr = 1;     /* last period's (t = 1) HPR */
    
    /* if sampling, get a random permutation between 0 and nPr-1,
     * else use the current index value. */
    I = (i_sample > 0) ? ( unif_rand() * (i_sample-1) ) : i;

    /* set the permutation locations for index 'I' */
    for(j=i_horizon; j--;) {
      i_perm[j] = (long)fmod(I/pow(nr,j),nr);
    }
    /* Keep track of this permutation's probability */
    probPerm[0] = d_prob[i_perm[0]];
    for(j=1; j<i_horizon; j++) {
      probPerm[j] = probPerm[j-1] * d_prob[i_perm[j]];
    }
    /* calculate each permutation's HPR if lsp object has no z values */
    if( using_z ) {
      /* call lspm::hpr and assign pointer */
      PROTECT(phpr = hpr(lsp, ScalarLogical(TRUE), perm));
      d_phpr = REAL(phpr);

      for(j=0; j<i_horizon; j++) {   /* loop over permutation locations */
        t1hpr *= d_phpr[j];          /* New portfolio balance */
        if( t1hpr <= d_dd ) {        /* if ruin % or max drawdown is hit */
          for(k=j; k<i_horizon; k++)
            d_outFail[k] += probPerm[k];
          break;
        }
        /* If calculating risk drawdown and last period was a new high */
        if( !i_ruin && t1hpr > 1 ) t1hpr = 1;
      }
      UNPROTECT(1);  /* UNPROTECT phpr */
    } else {
      for(j=0; j<i_horizon; j++) {   /* loop over permutation locations */
        t1hpr *= d_phpr[i_perm[j]];  /* New portfolio balance */
        if( t1hpr <= d_dd ) {        /* if ruin % or max drawdown is hit */
          for(k=j; k<i_horizon; k++)
            d_outFail[k] += probPerm[k];
          break;
        }
        /* If calculating risk drawdown and last period was a new high */
        if( !i_ruin && t1hpr > 1 ) t1hpr = 1;
      }
    }

    /* Total probability of all permutations */
    for(j=0; j<i_horizon; j++) {
      d_outSum[j] += probPerm[j];
    }
  }
  if(i_sample > 0) PutRNGstate();  /* Write out .Random.seed */

  /* Store results */
  const char *result_names[] = {"fail", "sum", ""};
  PROTECT(result = mkNamed(VECSXP, result_names)); P++;
  SET_VECTOR_ELT(result, 0, outFail);
  SET_VECTOR_ELT(result, 1, outSum);

  UNPROTECT(P);
  return result;
}

