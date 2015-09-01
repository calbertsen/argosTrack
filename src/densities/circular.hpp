#ifndef _CIRCULAR_DISTRIBUTIONS_
#define _CIRCULAR_DISTRIBUTIONS_

/** \brief Probability density function of the wrapped Cauchy distribution.
    \param mu Location parameter
    \param rho Dispersion parameter
*/
template <class Type>
Type dwcauchy(Type x, Type mu, Type gamma, int give_log = 0){
  Type logres = log(sinh(gamma)) - log(2*M_PI) - log(cosh(gamma)-cos(x-mu));
  if(give_log){
    return CppAD::CondExpLe(gamma,Type(0.0),Type(-INFINITY),logres);
  }else{
    return CppAD::CondExpLe(gamma,Type(0.0),Type(0.0),exp(logres));
  }

}

VECTORIZE3_tti(dwcauchy);


template <class Type>
Type dvonmises(Type x, Type mu, Type kappa, int give_log = 0){
  Type logres = kappa * cos(x - mu) - log(2*M_PI) - log(besselK(kappa, Type(0)));
  if(give_log) return logres; else return exp(logres);
}

VECTORIZE3_tti(dvonmises);

#endif
