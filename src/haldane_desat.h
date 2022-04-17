#ifndef desat_hald
#define desat_hald

#include <Rcpp.h>
using namespace Rcpp;

NumericVector cpp_half_life(NumericVector period, NumericVector time);

DataFrame cpp_haldane_desat(
  DataFrame dtcurve, NumericVector comp, NumericVector Scomp, 
  NumericVector depths, 
  NumericVector ppn2_ini = 0.791, NumericVector bpal_speed = 6.0
);

#endif
