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
  
  double *d_fval    = REAL(coerceVector(VECTOR_ELT(lsp, 2),REALSXP));
  double *d_maxloss = REAL(coerceVector(VECTOR_ELT(lsp, 3),REALSXP));

  double *d_f       = REAL(coerceVector(f,REALSXP));
  double *d_cFunVal = REAL(coerceVector(constrVal,REALSXP));
  double *d_margin, d_equity, maxU;
      
  int len = length(f);

  /* is changing 'lsp' stupid / dangerous? */
  for(int i=0; i < len; i++) {
    d_fval[i] = d_f[i];
  }

  SEXP s_ghpr, s_cval, fcall;
  /* Calculate GHPR */
  PROTECT(s_ghpr = ghpr(lsp)); P++;
  double d_ghpr = -REAL(s_ghpr)[0];

  if(d_ghpr < -1) {
    /* Margin constraint */
    if( !isNull(margin) && !isNull(equity) ) {

      d_margin = REAL(coerceVector(margin,REALSXP));
      d_equity = REAL(coerceVector(equity,REALSXP))[0];

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

      if( REAL(coerceVector(s_cval,REALSXP))[0] >= d_cFunVal[0] ) {
        d_ghpr = R_PosInf;
      }
    }
  } else {
    d_ghpr = R_PosInf;
  }

  UNPROTECT(P);
  return(ScalarReal(d_ghpr));
}

