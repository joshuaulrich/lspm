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

SEXP hpr ( SEXP lsp, SEXP port, SEXP order )
{
  /* Arguments:
   *   lsp      A 'lsp' class object
   *   port     Calculate portfolio HPR?
   *   order    Vector specifying order to evaluate events
   */

  int P=0;  /* PROTECT counter */
  int i, j;  /* loop counters */
  
  if(!inherits(lsp, "lsp")) error("not a 'lsp' object");
      
  /* extract lsp components */
  double *d_event   = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 0)))); P++;
  //double *d_prob    = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 1)))); P++;
  double *d_fval    = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 2)))); P++;
  double *d_maxloss = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 3)))); P++;
  double *d_zval    = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 4)))); P++;

  /* dimensions of events */
  int nc = ncols(VECTOR_ELT(lsp, 0));
  int nr = nrows(VECTOR_ELT(lsp, 0));

  int *i_order;
  if(isNull(order)) {  /* create order sequence */
    PROTECT(order = allocVector(INTSXP, nr)); P++;
    i_order = INTEGER(order);
    for(i=nr; i--;) i_order[i] = i;
  } else {             /* user-provided order sequence */
    i_order = INTEGER(PROTECT(AS_INTEGER(order))); P++;
  }
  int i_port = asInteger(port);
  int nro = nrows(order);

  /* if portfolio-level HPR is requested */
  int nc_res = (i_port == 1) ? 1 : nc;

  /* initialize result object and pointer */
  SEXP result;
  PROTECT(result = allocMatrix(REALSXP, nro, nc_res)); P++;
  double *d_result = REAL(result);

  /* general HPR incorporating z */
  double e0 = 1, e1 = 1;  /* equity values      */
  double zz, mul = 1;     /* exponent, multiple */
  double hpr, hprport;

  /* does the lsp object have non-zero z values? */
  int using_z = (d_zval[0]==0 && d_zval[1]==0) ? 0 : 1;

  /* loop over events in 'order' */
  for(j=0; j < nro; j++) {
    if(e1<=0) {  /* zero-fill hpr if equity < 0 */
      d_result[j] = 0;
      continue;
    }
    if(using_z) {
      /* calculate martingale exponent */
      zz = (e1/e0) < (1+d_zval[2]) ? d_zval[0] : d_zval[1];
      /* calculate multiplier based on zz */
      mul = pow(e0/e1, 1/(zz+1)-1);
    }
    hprport = 0;
    /* loop over each system HPR for this period */
    for(i=0; i < nc; i++) {
      /* calculate HPR */
      hpr = d_fval[i] * d_event[i_order[j]+i*nr] / -d_maxloss[i] * mul;
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

SEXP ghpr ( SEXP lsp )
{
  if(!inherits(lsp, "lsp")) error("not a 'lsp' object");

  int P=0;  /* PROTECT counter */
  
  /* extract lsp components */
  //double *d_event   = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 0)))); P++;
  double *d_prob    = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 1)))); P++;
  //double *d_fval    = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 2)))); P++;
  //double *d_maxloss = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 3)))); P++;
  //double *d_zval    = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 4)))); P++;

  /* calculate portfolio HPR */
  SEXP phpr;
  PROTECT(phpr = hpr(lsp, ScalarLogical(TRUE), R_NilValue)); P++;
  double *d_phpr = REAL(phpr);

  int nr = nrows(phpr);
  double sumProb = 0, result = 1;

  /* calculate GHPR from portfolio HPR */
  for(int i=0; i < nr; i++) {
    result *= pow(d_phpr[i], d_prob[i]);
    sumProb += d_prob[i];
  }
  result = pow(result, 1/sumProb);

  UNPROTECT(P);
  return(ScalarReal(result));
}
