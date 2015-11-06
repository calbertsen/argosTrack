
#ifndef _GEOLOCVARMAT_
#define _GEOLOCVARMAT_

template<class Type>
matrix<Type> geolocVarMat(Type sdObslon, Type dayOfYear, tmbutils::splinefun<Type> logSplLat){

  matrix<Type> res(2,2);
  res.setZero();
  res(1,1) = pow(sdObslon, Type(2.0));
  res(0,0) = exp(2.0 * logSplLat(dayOfYear));

  return res;  
}











#endif
