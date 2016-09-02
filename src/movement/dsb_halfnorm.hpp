
#ifndef _DSB_HALFNORM_
#define _DSB_HALFNORM_

// Disrete time steplength/bearing model:
// McClintock et. al. Movement Ecology 2014, 2:21
template<class Type>
Type nll_dsb_halfnorm(Type st, Type phit, Type phitm, Type rho, Type sd){

  Type res = -densities::dhalfnorm(st, sd, true);
  // rho must be positive
  res -= densities::dwcauchy(phit,phitm,rho,true);
  // res -= dvonmises(phit,phitm,rho,true);
  res -= log(st);
  
  return res;
}



#endif
