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
#include <Rinternals.h>
#include "lspm.h"

SEXP hpr ( SEXP lsp, SEXP port, SEXP order )
{
  /* Arguments:
   *   lsp      A 'lsp' class object
   *   port     Calculate portfolio HPR?
   *   order    Vector specifying order to evaluate events
   */

  int P=0;  /* PROTECT counter */
  int i, j, k;  /* loop counters */
  
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

  /* ensure 'port' is logical */
  if(TYPEOF(port) != LGLSXP) {
    PROTECT(port = coerceVector(port, LGLSXP)); P++;
  }
  int i_port = INTEGER(port)[0];

  /* ensure 'order' is integer */
  if(TYPEOF(order) != INTSXP) {
    PROTECT(order = coerceVector(order, INTSXP)); P++;
  }
  int *i_order = INTEGER(order);

  /* dimensions of events */
  int nc = ncols(event);
  int nr = nrows(event);
  int nro = nrows(order);

  /* if portfolio-level HPR is requested */
  int nc_res = (i_port == 1) ? 1 : nc;

  /* initialize result object and pointer */
  SEXP result;
  PROTECT(result = allocMatrix(REALSXP, nro, nc_res)); P++;
  double *d_result = REAL(result);

  /* general HPR incorporating z */
  double e0 = 1, e1 = 1;  /* equity values      */
  double zz, mul;         /* exponent, multiple */
  double hpr, hprport;

  /* loop over events in 'order' */
  for(j=0; j < nro; j++) {
    if(e1<=0) {
      d_result[j] = 0;
      continue;
    }
    /* extract the event location */
    k = i_order[j];
    /* calculate martingale exponent */
    zz = (e1/e0) < (1+d_zval[2]) ? d_zval[0] : d_zval[1];
    /* calculate multiplier based on zz */
    mul = pow(e0/e1, 1/(zz+1)-1);
    hprport = 0;
    /* loop over each system HPR for this period */
    for(i=0; i < nc; i++) {
      /* calculate HPR */
      hpr = d_fval[i] * d_event[k+i*nr] / -d_maxloss[i] * mul;
      /* add each system HPR to total portfolio HPR */
      hprport += hpr;
      if( !i_port ) {
        /* set HPR for each system */
        d_result[j+i*nr] = hpr + 1;
      }
    }
    if( i_port ) {
      /* set portfolio-level HPR only and ensure all HPR >= 0 */
      d_result[j] = (1+hprport) < 0 ? 0 : (1+hprport);
    }
    /* add ending equity to total */
    e1 += e1 * hprport;
  }

  UNPROTECT(P);
  return result;
}

