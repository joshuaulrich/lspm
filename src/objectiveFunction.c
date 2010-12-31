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

SEXP objFun_optimalf ( SEXP f, SEXP lsp, SEXP margin, SEXP equity,
  SEXP constrFun, SEXP constrVal, SEXP env )
{
  int P=0;
  
  double *d_fval    = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 2)))); P++;
  double *d_maxloss = REAL(PROTECT(AS_NUMERIC(VECTOR_ELT(lsp, 3)))); P++;

  double *d_f       = REAL(PROTECT(AS_NUMERIC(f))); P++;
  double *d_margin, d_equity, maxU;  /* -Wall */

  int len = length(f);

  /* is changing 'lsp' stupid / dangerous? */
  for(int i=0; i < len; i++) {
    d_fval[i] = d_f[i];
  }

  SEXP s_ghpr, s_cval, fcall;
  /* Calculate GHPR */
  PROTECT(s_ghpr = ghpr(lsp)); P++;
  double d_ghpr = -asReal(s_ghpr);

  if(d_ghpr < -1) {
    /* Margin constraint */
    if( !isNull(margin) && !isNull(equity) ) {

      d_margin = REAL(PROTECT(AS_NUMERIC(margin))); P++;
      d_equity = asReal(equity);

      maxU = 0;
      for(int i=0; i < len; i++) {
        maxU += d_f[i] * d_margin[i] / -d_maxloss[i];
      }
      maxU *= d_equity;

      if(maxU > d_equity) {
        d_ghpr = R_PosInf;
      }
    } /* Margin constraint */

    /* Constraint function */
    if( !isNull(constrFun) ) {

      if( !isFunction(constrFun) )
        error("constrFun is not a function");

      PROTECT(fcall = lang3(constrFun, lsp, R_DotsSymbol)); P++;
      PROTECT(s_cval = eval(fcall, env)); P++;

      if( asReal(s_cval) >= asReal(constrVal) ) {
        d_ghpr = R_PosInf;
      }
    }
  } else {
    d_ghpr = R_PosInf;
  }

  UNPROTECT(P);
  return(ScalarReal(d_ghpr));
}

