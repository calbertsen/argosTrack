
#ifndef _DTCRW_
#define _DTCRW_

// (Irregularized version of) Discrete Time Correlated Random Walk
template<class Type>
Type nll_dtcrw(vector<Type> mut, vector<Type> mutm, vector<Type> mutmm, Type gamma, Type phi, Type rho, vector<Type> varState){


  matrix<Type> cov(2,2);
  cov(0,0) = varState(0);
  cov(1,1) = varState(1);
  cov(1,0) = rho * sqrt(varState(0) * varState(1));
  cov(0,1) = rho * sqrt(varState(0) * varState(1));

  vector<Type> state(2);
  state(0) = mut(0) - (mutm(0) + gamma*(cos(phi)*(mutm(0)-mutmm(0)) - sin(phi)*(mutm(1)-mutmm(1))));
  state(1) = mut(1) - (mutm(1) + gamma*(sin(phi)*(mutm(0)-mutmm(0)) + cos(phi)*(mutm(1)-mutmm(1))));
  
  return density::MVNORM<Type>(cov)(state);
}

// For time t == 1
template<class Type>
Type nll_dtcrw1(vector<Type> mut, vector<Type> mutm, Type gamma, Type phi, Type rho, vector<Type> varState){


  matrix<Type> cov(2,2);
  cov(0,0) = varState(0);
  cov(1,1) = varState(1);
  cov(1,0) = rho * sqrt(varState(0) * varState(1));
  cov(0,1) = rho * sqrt(varState(0) * varState(1));

  vector<Type> state(2);
  state(0) = mut(0) - mutm(0);
  state(1) = mut(1) - mutm(1);
  
  return density::MVNORM<Type>(cov)(state);
}



#endif
