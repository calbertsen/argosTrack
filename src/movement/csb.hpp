
#ifndef _CSB_
#define _CSB_

// Continuous time steplength/bearing model:
// Parton et. al. https://arxiv.org/abs/1608.05583
template<class Type>
Type nll_csb(Type st, Type stm, Type phit, Type phitm, Type beta, Type gamma, Type sdS, Type sdB, Type dt){

  Type res = -dnorm(phit,phitm, sqrt(dt)*sdB, true);
  Type state = gamma + exp(-beta * dt) * (stm - gamma);
  Type var = Type(0.5) * sdS * sdS / beta * (Type(1.0) - exp(-Type(2.0) * beta * dt));
  res -= dnorm(st,dt * state, dt * sqrt(var),true);
  Rcout << st << "  " << stm << "  " << state << "  " << var << "\n";
  return res;
}


#endif
