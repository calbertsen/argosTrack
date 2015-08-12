
#ifndef _DSB_
#define _DSB_

// Disrete time steplength/bearing model:
// McClintock et. al. Movement Ecology 2014, 2:21
template<class Type>
Type nll_dsb(Type st, Type phit, Type phitm, Type rho, Type scale, Type shape){

  Type res = -dweibull(st, shape, scale, true);
  // rho must be between -1 and 1
  res -= dwcauchy(phit,phitm,rho,true);
  
  return res;
}



#endif
