#ifndef _CIRCULAR_DISTRIBUTIONS_
#define _CIRCULAR_DISTRIBUTIONS_

/** \brief Probability density function of the wrapped Cauchy distribution.
    \param mu Location parameter
    \param rho Dispersion parameter
*/
template <class Type>
Type dwcauchy(Type x, Type mu, Type gamma, int give_log){
  Type log_res = log(sinh(gamma)) - log(2*M_PI) - log(cosh(gamma)-cos(x-mu));
  if(give_log){
    return CppAD::CondExpLe(gamma,Type(0.0),Type(-INFINITY),log_res);
  }else{
    return CppAD::CondExpLe(gamma,Type(0.0),Type(0.0),exp(log_res));
  }

}

VECTORIZE3_tti(dwcauchy);

#endif
