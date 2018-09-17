
#ifndef _COORDINATE_TRANSFORMATIONS_
#define _COORDINATE_TRANSFORMATIONS_

// Transform latitude/longitude coordinates to nautical miles
template<class Type>
vector<Type> ll2n(Type lon, Type lat){
  vector<Type> res(2);
  res(0) = lon * Type(60.0) * cos(lat * M_PI / Type(180.0));
  res(1) = lat * Type(60.0);
  return res;
}

// Transform nautical miles to latitude/longitude
template<class Type>
vector<Type> n2ll(Type x, Type y){
  vector<Type> res(2);
  res(0) = x / (Type(60.0) * cos(y * M_PI / Type(180.0) / Type(60.0)));
  res(1) = y / Type(60.0);
  return res;
}



// Export to R
extern "C" {
  SEXP ll2n(SEXP lon, SEXP lat){
    if(!(isNumeric(lon) && Rf_length(lon)==1))error("lon must be a numeric of length 1");
    if(!(isNumeric(lat) && Rf_length(lat)==1))error("lat must be a numeric of length 1");
    vector<double> res = ll2n(REAL(lon)[0],REAL(lat)[0]);
    SEXP ress = PROTECT(allocVector(REALSXP,res.size()));
    for(int i = 0; i < res.size(); ++i)
      REAL(ress)[i] = res(i);
    UNPROTECT(1);
    return ress;
  }
  SEXP n2ll(SEXP x, SEXP y){
    if(!(isNumeric(x) && Rf_length(x)==1))error("x must be a numeric of length 1");
    if(!(isNumeric(y) && Rf_length(y)==1))error("y must be a numeric of length 1");
    vector<double> res = n2ll(REAL(x)[0],REAL(y)[0]);
    SEXP ress = PROTECT(allocVector(REALSXP,res.size()));
    for(int i = 0; i < res.size(); ++i)
      REAL(ress)[i] = res(i);
    UNPROTECT(1);
    return ress;
  }
}
#endif
