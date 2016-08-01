
#ifndef _IDTCRW_
#define _IDTCRW_

// Irregular Discrete Time Correlated Random Walk
template<class Type>
Type nll_idtcrw(vector<Type> mut, vector<Type> mutm, vector<Type> mutmm, Type dt, vector<Type> gamma, Type phi, Type rho, vector<Type> mupar, vector<Type> varState){


  matrix<Type> cov(2,2);
  cov(0,0) = varState(0);
  cov(1,1) = varState(1);
  cov(1,0) = rho * sqrt(varState(0) * varState(1));
  cov(0,1) = rho * sqrt(varState(0) * varState(1));

  matrix<Type> Gth(2,2);
  Gth.setZero();
  Gth(0,0) = -log(gamma(0));
  Gth(1,1) = -log(gamma(0));
  Gth(0,1) = phi;
  Gth(1,0) = -phi;
  // Gth << -log(gamma), -phi, phi, -log(gamma);
  matrix<Type> Gks = kroneckersum(Gth,Gth);
  matrix<Type> I(Gks.cols(),Gks.cols());
  I.setIdentity();

  // matrix<Type> meGth = expm((matrix<Type>)(-Gth * dt));
  matrix<Type> meGth(2,2);
  meGth.setZero();
  vector<Type> pgt = pow(gamma,dt);
  Type tph = dt * phi;
  meGth(0,0) = pgt(0) * cos(tph);
  meGth(0,1) = -pgt(0) * sin(tph);
  meGth(1,0) = pgt(0) * sin(tph);
  meGth(1,1) = pgt(1) * cos(tph);
  
  // matrix<Type> meGks = expm((matrix<Type>)(-Gks * dt));
  matrix<Type> meGks(4,4);
  meGks.setZero();
  Type pg2t = pow(pgt(0),2.0);
  Type s2tph = sin(2.0 * dt * phi);
  Type c2tph = cos(2.0 * dt * phi);
  
  meGks(0,0) = 0.5 * pg2t * ( c2tph + 1.0 );
  meGks(0,1) = -0.5 * pg2t * s2tph;
  meGks(0,2) = -0.5 * pg2t * s2tph;
  meGks(0,3) = -0.5 * pg2t * ( c2tph - 1.0 );

  meGks(1,0) = 0.5 * pg2t * s2tph;
  meGks(1,1) = 0.5 * pg2t * ( c2tph + 1.0 );
  meGks(1,2) = 0.5 * pg2t * ( c2tph - 1.0 );
  meGks(1,3) = -0.5 * pg2t * s2tph;

  meGks(2,0) = 0.5 * pg2t * s2tph;
  meGks(2,1) = 0.5 * pg2t * ( c2tph - 1.0 );
  meGks(2,2) = 0.5 * pg2t * ( c2tph + 1.0 );
  meGks(2,3) = -0.5 * pg2t * s2tph;

  meGks(3,0) = 0.5 * pg2t * ( c2tph - 1.0 );
  meGks(3,1) = 0.5 * pg2t * s2tph;
  meGks(3,2) = 0.5 * pg2t * s2tph;
  meGks(3,3) = 0.5 * pg2t * ( c2tph + 1.0 );
  
  matrix<Type> covVec(4,1);
  covVec = cov.vec();
  
  
  vector<Type> varVec = ((matrix<Type>)Gks.inverse()) * (matrix<Type>)((I - meGks) * covVec);
  matrix<Type> var = asMatrix(varVec,2,2);
  
  // matrix<Type> var(2,2);
  // var.setZero();
  // var(0,0) = abs(0.5*(cov(0,0)*cos(phi*dt)*sin(phi*dt)+cov(1,1)*cos(phi*dt)*sin(phi*dt)+cov(0,0)*phi*dt-cov(1,1)*phi*dt)/phi);
  // var(0,1) = 0.0; // This should be different
  // var(1,0) = var(0,1);
  // var(1,1) = abs(0.5*(cov(0,0)*cos(phi*dt)*sin(phi*dt)+cov(1,1)*cos(phi*dt)*sin(phi*dt)-cov(0,0)*phi*dt+cov(1,1)*phi*dt)/phi);
  // vector<Type> state = mut - (mutm + (vector<Type>)(meGth(-dt) * (mutm - mutmm).matrix()));
  vector<Type> state = mut - (mutm + mupar + (vector<Type>)(meGth * (mutm - mutmm - mupar).matrix()));
  
  //return density::MVNORM<Type>(asMatrix(var,2,2))(state);
  return density::MVNORM<Type>(var)(state);
}

// For time t == 1
template<class Type>
Type nll_idtcrw1(vector<Type> mut, vector<Type> mutm, Type dt, vector<Type> gamma, Type phi, Type rho, vector<Type> mupar, vector<Type> varState){


  matrix<Type> cov(2,2);
  cov(0,0) = varState(0);
  cov(1,1) = varState(1);
  cov(1,0) = rho * sqrt(varState(0) * varState(1));
  cov(0,1) = rho * sqrt(varState(0) * varState(1));

  vector<Type> state(2);
  state(0) = mut(0) - mutm(0) + mupar(0);
  state(1) = mut(1) - mutm(1) + mupar(1);
  
  return density::MVNORM<Type>(cov)(state);
}



#endif
