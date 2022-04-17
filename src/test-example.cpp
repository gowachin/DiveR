/*
 * This file uses the Catch unit testing library, alongside
 * testthat's simple bindings, to test a C++ function.
 *
 * For your own packages, ensure that your test files are
 * placed within the `src/` folder, and that you include
 * `LinkingTo: testthat` within your DESCRIPTION file.
 */

// All test files should include the <testthat.h>
// header file.
#include <testthat.h>
#include "haldane_desat.h"
#include "insertrow.h"

context("insertCell") {
  
  test_that("insertCell num") {
    int i = 1;
    SEXP v = PROTECT(Rf_allocVector(REALSXP, 4));
    for (int i = 0; i < 4; i++) {
      REAL(v)[i] = i + 1;
    }
    SEXP r = PROTECT(Rf_allocVector(REALSXP, 5));
    for (int i = 0; i < 5; i++) {
      REAL(r)[i] = i;
    }
    REAL(r)[1] = 1;
    
    SEXP d = insertCell(v, i);
    // expect_true(d == r); 
    // TODO : test this function...
    
    UNPROTECT(2);
  }
  
}


context("Haldane half_life") {

  test_that("testing half_life") {
    NumericVector p = 1;
    NumericVector t = 1;
    NumericVector r = 50;
    
    // expect_true(cpp_half_life(p, t) == r);
  }

}
