
#ifndef _CTCRW_
#define _CTCRW_


// Continuous-time correlated random walk
template<class Type>
Type nll_ctcrw(vector<Type> mut, vector<Type> mutm, vector<Type> velt, vector<Type> veltm,Type dt, vector<Type> beta, vector<Type> gamma, vector<Type> varState){

  vector<Type> state(4);
  state.setZero();
  matrix<Type> cov(4,4);
  cov.setZero();

  state(0) = mut(0)-(mutm(0)+veltm(0)*(1.0-exp(-beta(0)*dt))/beta(0));
  state(1) = velt(0) - (gamma(0)+exp(-beta(0)*dt)*(veltm(0)-gamma(0)));

  state(2) = mut(1)-(mutm(1)+veltm(1)*(1.0-exp(-beta(1)*dt))/beta(1));
  state(3) = velt(1) - (gamma(1)+exp(-beta(1)*dt)*(veltm(1)-gamma(1)));

  cov(0,0) = varState(0)/pow(beta(0),2.0)*(dt-2.0*(1.0-exp(-beta(0)*dt))/beta(0)+(1.0-exp(-2.0*beta(0)*dt))/(2.0*beta(0)));
  cov(1,1) = varState(0)*(1.0-exp(-2.0*beta(0)*dt))/(2.0*beta(0));
  cov(1,0) = varState(0)*(1.0-2.0*exp(-beta(0)*dt)+exp(-2.0*beta(0)*dt))/(2.0*pow(beta(0),2.0));
  cov(0,1) = cov(1,0);
      
  cov(2,2) = varState(1)/pow(beta(1),2.0)*(dt-2.0*(1.0-exp(-beta(1)*dt))/beta(1)+(1.0-exp(-2.0*beta(1)*dt))/(2.0*beta(1)));
  cov(3,3) = varState(1)*(1.0-exp(-2.0*beta(1)*dt))/(2.0*beta(1));
  cov(2,3) = varState(1)*(1.0-2.0*exp(-beta(1)*dt)+exp(-2.0*beta(1)*dt))/(2.0*pow(beta(1),2.0));
  cov(3,2) = cov(2,3);
	
  return density::MVNORM<Type>(cov)(state);
}



#endif

