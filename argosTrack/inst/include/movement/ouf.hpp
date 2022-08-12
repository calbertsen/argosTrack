
#ifndef _OUF_
#define _OUF_


// Ornstein-Uhlenbeck with foraging
template<class Type>
Type nll_ouf(vector<Type> mut, vector<Type> mutm, vector<Type> velt, vector<Type> veltm,Type dt, vector<Type> beta, vector<Type> gamma, vector<Type> varState){

  vector<Type> state(4);
  state.setZero();
  matrix<Type> cov(4,4);
  cov.setZero();

  state(0) = mut(0) - (mutm(0) - beta(0) * (mutm(0) - gamma(0)) + veltm(0)) * dt;
  state(1) = mut(1) - (mutm(1) - beta(1) * (mutm(1) - gamma(1)) + veltm(1)) * dt;

  state(2) = velt(0) - (exp(-beta(2)*dt)*(veltm(0))); // Euler: velt(0) - (veltm(0) - beta(2) * veltm(0) ) * dt;
  state(3) = velt(1) - (exp(-beta(3)*dt)*(veltm(1))); // velt(1) - (veltm(0) - beta(3) * veltm(1) ) * dt;

  Type v1 = varState(0)*(1.0-exp(-2.0*beta(2)*dt))/(2.0*beta(2));
  Type v2 = varState(1)*(1.0-exp(-2.0*beta(3)*dt))/(2.0*beta(3));
  
  cov(0,0) = 0.001 * v1;
  cov(1,1) = 0.001 * v2;
      
  cov(2,2) = 0.999 * v1;
  cov(3,3) = 0.999 * v2;;
	
  return density::MVNORM<Type>(cov)(state);
}



#endif

