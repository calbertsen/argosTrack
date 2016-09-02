
#ifndef _OUV_
#define _OUV_

// Ornstein-Uhlenbeck Velocity model
template<class Type>
Type nll_ouv(vector<Type> mut, vector<Type> mutm, vector<Type> mutmm, Type dt, vector<Type> gamma, Type rho, vector<Type> mupar, vector<Type> varState){


  matrix<Type> cov(2,2);
  cov(0,0) = varState(0);
  cov(1,1) = varState(1);
  cov(1,0) = rho * sqrt(varState(0) * varState(1));
  cov(0,1) = rho * sqrt(varState(0) * varState(1));

  matrix<Type> Gth(2,2);
  Gth.setZero();
  Gth(0,0) = gamma(0);
  Gth(1,1) = gamma(3);
  Gth(0,1) = gamma(2);
  Gth(1,0) = gamma(1);

  matrix<Type> meGth = expm((matrix<Type>)(-Gth * dt));
  vector<Type> state = mut - (mutm + mupar + (vector<Type>)(meGth * (mutm - mutmm - mupar).matrix()));

  matrix<Type> Gks = convenience::kroneckersum(Gth,Gth);
  
  vector<Type> varVec = (matrix<Type>)Gks.inverse() * cov.vec().matrix();
  matrix<Type> varMat = asMatrix(varVec,2,2);
  
  
  matrix<Type> var = varMat - (matrix<Type>)(meGth * (matrix<Type>)(varMat * (matrix<Type>)meGth.transpose()));
  
  return density::MVNORM<Type>(var)(state);
}

// For time t == 1
template<class Type>
Type nll_ouv1(vector<Type> mut, vector<Type> mutm, Type dt, vector<Type> gamma, Type rho, vector<Type> mupar, vector<Type> varState){


  matrix<Type> cov(2,2);
  cov(0,0) = varState(0);
  cov(1,1) = varState(1);
  cov(1,0) = rho * sqrt(varState(0) * varState(1));
  cov(0,1) = rho * sqrt(varState(0) * varState(1));

  matrix<Type> Gth(2,2);
  Gth.setZero();
  Gth(0,0) = gamma(0);
  Gth(1,1) = gamma(3);
  Gth(0,1) = gamma(2);
  Gth(1,0) = gamma(1);

  matrix<Type> Gks = convenience::kroneckersum(Gth,Gth);
  
  vector<Type> varVec = (matrix<Type>)Gks.inverse() * cov.vec().matrix();
  matrix<Type> varMat = asMatrix(varVec,2,2);
 

  vector<Type> state(2);
  state(0) = mut(0) - mutm(0) + mupar(0);
  state(1) = mut(1) - mutm(1) + mupar(1);
  
  return density::MVNORM<Type>(varMat)(state);
}



#endif
