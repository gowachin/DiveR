#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

template <int RTYPE>
Vector<RTYPE> insertCell(const Vector<RTYPE>& x, int n)
{
  n = std::max(n, 1);
  n = std::min((R_xlen_t)n , x.size());
  int s = x.size() + 1;
  Vector<RTYPE> res(s);
  
  std::copy(x.begin(), x.begin() + n, res.begin());
  std::copy(x.begin() + (n-1), x.end(), res.begin() + n );
  
  return res;
}

} // impl

// [[Rcpp::export]]
SEXP insertCell(SEXP x, int n) {
  switch (TYPEOF(x)) {
    case INTSXP: {
      return impl::insertCell(as<IntegerVector>(x), n);
    }
    case REALSXP: {
      return impl::insertCell(as<NumericVector>(x), n);
    }
    case STRSXP: {
      return impl::insertCell(as<CharacterVector>(x), n);
    }
    case LGLSXP: {
      return impl::insertCell(as<LogicalVector>(x), n);
    }
    case CPLXSXP: {
      return impl::insertCell(as<ComplexVector>(x), n);
    }
    default: {
      warning(
        "Invalid SEXPTYPE %d (%s).\n",
        TYPEOF(x), type2name(x)
      );
    return R_NilValue;
    }
  }
}


void insertRow(DataFrame df, DataFrame newrow, NumericVector r){
  
  int nc = df.size();
  int nr = df.nrows();
  int R = r[0] - 1 ;
  bool bind = false;
  IntegerVector rws, trws, crws;
  
  // TODO : check if newrow and df share the type of column and ncol !
  
  if(R < 0){
    warning("R can't be negative value. A cbind has been performed.");
    R = 0;
  }
  if(R >= nr){
    if(R > nr){
      warning("R can't be superior to nrow(df)+1. A cbind has been performed.");
    }
    bind = true;
    R = nr;
  }
  
  if(R-1 < 0){ rws = 0; } else { rws = Rcpp::Range(0, R-1); }
  if(nr < R+1){ trws = R+1; } else { trws = Rcpp::Range(R+1, nr); }
  if(nr-1 < R){ crws = R; } else { crws = Rcpp::Range(R, nr-1); }
  
  df.attr("class") = "list";
  for(int i =0; i < nc; i ++){
    if(is<NumericVector>(df[i])){
      NumericVector tmp (nr+1);
      NumericVector col = df[i];
      NumericVector nwcol = newrow[i];
      
      tmp[rws] = col[rws];
      if(!bind){ tmp[trws] = col[crws]; }
      tmp[R] = nwcol[0];
      
      df[i] = tmp;
      
    } else if(is<IntegerVector>(df[i])){
      IntegerVector tmp (nr+1);
      IntegerVector col = df[i];
      IntegerVector nwcol = newrow[i];
      
      tmp[rws] = col[rws];
      if(!bind){ tmp[trws] = col[crws]; }
      tmp[R] = nwcol[0];
      
      df[i] = tmp;
      
    } else if(is<CharacterVector>(df[i])){
      CharacterVector tmp (nr+1);
      CharacterVector col = df[i];
      CharacterVector nwcol = newrow[i];
      
      tmp[rws] = col[rws];
      if(!bind){ tmp[trws] = col[crws]; }
      tmp[R] = nwcol[0];
      
      df[i] = tmp;
      
    } else if(is<LogicalVector>(df[i])){
      LogicalVector tmp (nr+1);
      LogicalVector col = df[i];
      LogicalVector nwcol = newrow[i];
      
      tmp[rws] = col[rws];
      if(!bind){ tmp[trws] = col[crws]; }
      tmp[R] = nwcol[0];

      df[i] = tmp;
      
    }
  }
  df.attr("class") = "data.frame";
  df.attr("row.names") = Rcpp::Range(1, nr+1);
}


// [[Rcpp::export]]
DataFrame cpp_insertRow(
    DataFrame df, DataFrame newrow, NumericVector r
){
  DataFrame dfr = clone(df);
  insertRow(dfr, newrow, r);
  return(dfr);
}
