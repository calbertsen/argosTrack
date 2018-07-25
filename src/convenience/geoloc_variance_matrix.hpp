
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



template<class Type>
matrix<Type> geolocVarMatFormula(Type sdObslon, Type dayOfYear, Type sdObsLat, Type sdObsLatExtra){

  matrix<Type> res(2,2);
  res.setZero();
  res(1,1) = pow(sdObslon, Type(2.0));
  Type theta = 2.0 * Type(M_PI) * (dayOfYear + sdObsLatExtra) / 365.25;
  res(0,0) = pow(sdObsLat, Type(2.0)) * 1.0 / pow(cos(theta),Type(2.0));

  return res;  
}







#endif
