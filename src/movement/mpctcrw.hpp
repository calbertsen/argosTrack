
#ifndef _MPCTCRW_
#define _MPCTCRW_

// Mixed memory continuous-time correlated random walk
template<class Type>
Type nll_mpctcrw(vector<Type> mut, vector<Type> mutm, vector<Type> velt, vector<Type> veltm,Type dt, vector<Type> beta, vector<Type> gamma, vector<Type> varState){

  vector<Type> state(6);
  matrix<Type> cov(6,6);
  cov.setZero();
  beta(2) += beta(0);
  beta(3) += beta(1);

  state(0) = mut(0)-(mutm(0)+veltm(0)*(1.0-exp(-beta(0)*dt))/beta(0)+veltm(2)*(1.0-exp(-beta(2)*dt))/beta(2));
  state(1) = velt(0) - (gamma(0)+exp(-beta(0)*dt)*(veltm(0)-gamma(0)));
  state(2) = velt(2) - (gamma(2)+exp(-beta(2)*dt)*(veltm(2)-gamma(2)));

  state(3) = mut(1)-(mutm(1)+veltm(1)*(1.0-exp(-beta(1)*dt))/beta(1)+veltm(3)*(1.0-exp(-beta(3)*dt))/beta(3));
  state(4) = velt(1) - (gamma(1)+exp(-beta(1)*dt)*(veltm(1)-gamma(1)));
  state(5) = velt(3) - (gamma(3)+exp(-beta(3)*dt)*(veltm(3)-gamma(3)));

  //cov(0,0) = varState(0)/pow(beta(0),2.0)*(dt-2.0*(1.0-exp(-beta(0)*dt))/beta(0)+(1.0-exp(-2.0*beta(0)*dt))/(2.0*beta(0)));
  //cov(0,0) += varState(2)/pow(beta(2),2.0)*(dt-2.0*(1.0-exp(-beta(2)*dt))/beta(2)+(1.0-exp(-2.0*beta(2)*dt))/(2.0*beta(2)));
  // cov(0,0) += dt*varState(4);
  cov(0,0) = 0.001;
  cov(1,1) = varState(0)*(1.0-exp(-2.0*beta(0)*dt))/(2*beta(0));
  cov(2,2) = varState(2)*(1.0-exp(-2.0*beta(2)*dt))/(2*beta(2));
  //cov(1,0) = varState(0)*(1.0-2.0*exp(-beta(0)*dt)+exp(-2.0*beta(0)*dt))/(2.0*pow(beta(0),2.0));
  cov(1,0) = 0.0;
  cov(0,1) = cov(1,0);
  //cov(2,0) = varState(2)*(1.0-2.0*exp(-beta(2)*dt)+exp(-2.0*beta(2)*dt))/(2.0*pow(beta(2),2.0));
  cov(2,0) = 0.0;
  cov(0,2) = cov(2,0);
  cov(2,1) = 0.0;
  cov(1,2) = cov(2,1);
      
  // cov(3,3) = varState(1)/pow(beta(1),2.0)*(dt-2.0*(1.0-exp(-beta(1)*dt))/beta(1)+(1.0-exp(-2.0*beta(1)*dt))/(2.0*beta(1)));
  // cov(3,3) += varState(3)/pow(beta(3),2.0)*(dt-2.0*(1.0-exp(-beta(3)*dt))/beta(3)+(1.0-exp(-2.0*beta(3)*dt))/(2.0*beta(3)));
  // cov(3,3) += dt*varState(5);
  cov(3,3) = 0.001;
  cov(4,4) = varState(1)*(1.0-exp(-2.0*beta(1)*dt))/(2.0*beta(1));
  cov(5,5) = varState(3)*(1.0-exp(-2.0*beta(3)*dt))/(2.0*beta(3));
  //cov(3,4) = varState(1)*(1.0-2.0*exp(-beta(1)*dt)+exp(-2.0*beta(1)*dt))/(2.0*pow(beta(1),2.0));
  cov(3,4) = 0.0;
  cov(4,3) = cov(3,4);
  //cov(3,5) = varState(3)*(1.0-2.0*exp(-beta(3)*dt)+exp(-2.0*beta(3)*dt))/(2.0*pow(beta(3),2.0));
  cov(3,5) = 0.0;
  cov(5,3) = cov(3,5);
  cov(4,5) = 0.0;
  cov(5,4) = cov(4,5);
	
  return density::MVNORM<Type>(cov)(state);
}


#endif
