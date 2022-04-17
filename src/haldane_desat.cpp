#include <Rcpp.h>
#include "insertrow.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector vecpow2(NumericVector base, NumericVector exp) {
  NumericVector out(exp.size());
  for(int i = 0; i < exp.size(); i++){
    out[i] = pow(base[0], exp[i]);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector vecdiv(const NumericVector num, const NumericVector denum) {
  
  if(num.size() == 1){
    NumericVector out(denum.size());
    for(int i = 0; i < denum.size(); i++){
      out[i] = num[0]  / denum[i];
    }
    return out;
  } 
  
  if(denum.size() == 1){
    NumericVector out(num.size());
    for(int i = 0; i < num.size(); i++){
      out[i] = num[i] /  denum[0];
    }
    return out;
  } 
  
  if(denum.size() == num.size()){
    NumericVector out;
    out = num / denum;
    return out;
  }
  
  NumericVector out = NumericVector::create(NA_REAL);
  
  return out;
}

//' half_life
//' 
//' @param period period in minute for the compartment.
//' @param time time at which user want the load of nitrogen
//' 
//' @examples 
//' cpp_half_life(period = 1, time = c(1,2))
//' 
//' @rdname half_life
//' @export
// [[Rcpp::export]]
NumericVector cpp_half_life(NumericVector period, NumericVector time){

  NumericVector n;
  NumericVector e = {2};

  n = (1 - vecpow2(e , - vecdiv(time, period))) * 100;
  
  return(n);
}

// [[Rcpp::export]]
DataFrame cpp_haldane_desat(
    DataFrame dtcurve, NumericVector comp, NumericVector Scomp, 
    NumericVector depths, 
    NumericVector ppn2_ini = 0.791, NumericVector bpal_speed = 6.0
){
  
  int ncomp = comp.length();
  
  NumericVector Scurve (ncomp, 1.0);
  NumericVector Ccurve (ncomp, ppn2_ini[0]);
  
  bool ne_pal = true; // limit warnings in loop
  bool bpal = false;
  
  DataFrame C_dtcurve = clone(dtcurve);
  
  NumericVector depth = C_dtcurve["depth"];
  NumericVector time = C_dtcurve["time"];
  NumericVector dt = C_dtcurve["dt"];
  NumericVector ppn2 = C_dtcurve["ppn2"];
  LogicalVector anarchy = C_dtcurve["anarchy"];
  NumericVector drive = C_dtcurve["drive"];
  NumericVector Pabs_pal = C_dtcurve["Pabs_pal"];
  NumericVector max_depth = C_dtcurve["max_depth"];
  NumericVector nex_pal = C_dtcurve["nex_pal"];
  LogicalVector need_pal = C_dtcurve["need_pal"];
  NumericVector time_pal = C_dtcurve["time_pal"];
  
  NumericVector tmp_Pabs_pal;
  NumericVector max_d, next_pal;
  NumericVector subdt, subppn2 (1);
  NumericVector min_palt (4);
  
  int i = 1;
  int imax = dtcurve.nrow() - 1;
  while(i <= imax){

    if(time_pal[i-1] > 0){
      depth = insertCell(depth, i);
      time = insertCell(time, i);
      dt = insertCell(dt, i);
      ppn2 = insertCell(ppn2, i);
      anarchy = insertCell(anarchy, i);
      drive = insertCell(drive, i);
      Pabs_pal = insertCell(Pabs_pal, i);
      max_depth = insertCell(max_depth, i);
      nex_pal = insertCell(nex_pal, i);
      need_pal = insertCell(need_pal, i);
      time_pal = insertCell(time_pal, i);
      
      dt[i] = time_pal[i];
      imax++;
      
      bpal = true;
    }
    
    // adapt speed if between palier
    if((depth[i-1] - depth[i]) / dt[i] > bpal_speed[0] && bpal){ 
      dt[i] = (depth[i-1] - depth[i]) / bpal_speed[0];
    } else if(depth[i-1] > depth[i]){
      bpal = false;
    }
    
    
    // compute new compart sat.
    subdt = dt[i];
    Ccurve = (rep(ppn2[i], ncomp) - Ccurve) * cpp_half_life(comp, subdt ) / rep(100.0, ncomp)  + Ccurve ;
    subppn2 =  ppn2[i]/ppn2[0];
    Scurve = Ccurve / rep(subppn2, ncomp); // S = Tn2 / Pabs
    anarchy[i] = is_true(any(Scurve >= Scomp)); // is any compartement anar // TODO : rm line
    
    
    tmp_Pabs_pal = Ccurve / Scomp; // Pabs = Tn2 / Sc compart
    drive[i] = which_max(tmp_Pabs_pal); 
    Pabs_pal[i] = max(tmp_Pabs_pal);
    max_depth[i] = (Pabs_pal[i] - 1) * 10;
    max_d[0] = max_depth[i];
    if( (max_d[0] < 0) | (round(max_d, 3)[0] == 0) ){
      max_depth[i] = 0;
      max_d[0] = 0;
    }
    
    
    // okay on c'est quand il faut un palier et sa profondeur !
    if(max_d[0] > max(depths)){
      nex_pal[i] = ceiling(max_d)[0];
      if(ne_pal){ // only set this warning once !
        warning("Saturation model limit reach with a ceiling depth below maximum deco stop depth. Ascent speed below 15m/min should cancel this issue.");
        ne_pal = FALSE;
      }
    } else {
      min_palt = depths[(rep(max_d[0], 4) - depths) <= 0];
      nex_pal[i] = Rcpp::min(min_palt);
    }
    
    // we matched the depth with next deco stop depth.
    need_pal[i] = depth[i] <= nex_pal[i]; 
    
    // only happens if need_pal ! will be but in another loop
    if(need_pal[i] && depth[i] != 0 ){
      next_pal = depths[(depths - rep(nex_pal[i] - 3, 4))  == 0]; // = max depth ???
      
      time_pal[i] = -comp[drive[i]] * (
            log(1 - (((Scomp[drive[i]] * (next_pal[0] / 10 +1) )- Ccurve[drive[i]])/
              (ppn2[i] - Ccurve[drive[i]]) )
            ) / log(2)
        );
    } else {
      time_pal[i] = 0; // TODO : why if this is the default ?
    }
    
    // TODO : probleme d'arrondis dans les diffs
    if(depth[i] != 0 && ((
      round(dt, 3)[i] > round(diff(time), 3)[i+1] && need_pal
    ) | bpal)){

      time[Range(i+1,time.size()-1)] = time[Range(i+1,time.size()-1)] +  dt[i];
      // TODO : ceiling decimal at same dec as cut interval !
    }
    
    i = i + 1;
  }
   
  DataFrame res_dtcurve = DataFrame::create( 
    Named("depth") = depth,
    Named("time") = time,
    Named("dt") = dt,
    Named("ppn2") = ppn2,
    Named("anarchy") = anarchy,
    Named("drive") = drive + 1,
    Named("Pabs_pal") = Pabs_pal,
    Named("max_depth") = max_depth,
    Named("nex_pal") = nex_pal,
    Named("need_pal") = need_pal,
    Named("time_pal") = time_pal
  );

  return(res_dtcurve);
}
