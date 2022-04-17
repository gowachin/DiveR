#ifndef insertrow
#define insertrow

#include <Rcpp.h>
using namespace Rcpp;

DataFrame cpp_insertRow(DataFrame df, DataFrame newrow, NumericVector r);
SEXP insertCell(SEXP x, int n);

#endif