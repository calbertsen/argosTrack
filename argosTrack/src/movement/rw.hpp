
#ifndef _RW_
#define _RW_

// Random walk
template<class Type>
Type nll_rw(vector<Type> mut, vector<Type> mutm, Type dt, vector<Type> varState){

  vector<Type> state(2);
  matrix<Type> cov(2,2);

  state = mut-mutm;
  cov.diagonal() = varState*dt;
	
  return density::MVNORM<Type>(cov)(state);
}


#endif
