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
#include <Rcpp.h>
#include "insertrow.h"


using namespace Rcpp;


// Initialize a unit test context. This is similar to how you
// might begin an R test file with 'context()', expect the
// associated context should be wrapped in braced.
context("rcpp_insertRows") {
  
  test_that("Adding a rows") {
    NumericVector n1 = {1.0,2.0,3.0};
    IntegerVector i1 = {1,2,3};
    CharacterVector c1 = {"A", "B", "C"};
    LogicalVector l1 = {false, true, false};
    DataFrame dfi = DataFrame::create( Named("V1") = n1 ,
                                       _["V2"] = i1, 
                                       _["V3"] = c1,
                                       _["V4"] = l1 ,
                                       Named("stringsAsFactors") = false);
    
    NumericVector n2 = {42.0};
    IntegerVector i2 = {42};
    CharacterVector c2 = {"I"};
    LogicalVector l2 = {true};
    DataFrame dfin = DataFrame::create( Named("V1") = n2 , 
                                        _["V2"] = i2, 
                                        _["V3"] = c2,
                                        _["V4"] = l2,
                                        Named("stringsAsFactors") = false);
    
    NumericVector n3 = {1.0,2.0,42.0,3.0};
    IntegerVector i3 = {1,2,42,3};
    CharacterVector c3 = {"A", "B", "I", "C"};
    LogicalVector l3 = {false, true, true, false};
    DataFrame dfe = DataFrame::create( Named("V1") = n3 ,
                                       _["V2"] = i3, 
                                       _["V3"] = c3,
                                       _["V4"] = l3,
                                       Named("stringsAsFactors") = false);
    
    NumericVector rr = {2};
    DataFrame res = cpp_insertRow(dfi, dfin, rr);
    
    Function identical("identical");
    
    LogicalVector tada = identical(res, dfe);
    
    expect_true(tada[0]);
  }

}
