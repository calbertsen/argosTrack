#ifndef _CIRCULAR_DISTRIBUTIONS_
#define _CIRCULAR_DISTRIBUTIONS_

/** \brief Probability density function of the wrapped Cauchy distribution.
    \param mu Location parameter
    \param rho Dispersion parameter
*/
template <class Type>
Type dwcauchy(Type x, Type mu, Type rho, int give_log){
  Type logval = log(Type(1.0)-pow(rho,Type(2.0))) - log(Type(2.0)*M_PI) - log(Type(1.0) + pow(rho,Type(2.0)) - Type(2.0) * rho * cos(x-mu));
  
  if(!give_log)
    return CppAD::CondExpGe(rho,Type(1),Type(0),
			    CppAD::CondExpLe(rho,Type(-1),Type(0),exp(logval)));
  else
    return CppAD::CondExpGe(rho,Type(1),Type(-INFINITY),
			    CppAD::CondExpLe(rho,Type(-1),Type(-INFINITY),exp(logval)));

}

VECTORIZE3_tti(dwcauchy);

#endif
