
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


#endif
