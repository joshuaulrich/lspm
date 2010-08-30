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
#include <Rdefines.h>

#ifndef _LSPM
#define _LSPM

// Functions
//SEXP nPri ( SEXP n, SEXP r, SEXP i, SEXP replace );
SEXP probRD ( SEXP beg, SEXP end, SEXP DD, SEXP horizon,
        SEXP lsp, SEXP ruin, SEXP sample );
SEXP hpr ( SEXP lsp, SEXP port, SEXP order );

#endif

