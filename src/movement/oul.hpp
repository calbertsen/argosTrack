
#ifndef _OUL_
#define _OUL_


// Ornstein-Uhlenbeck Location model
template<class Type>
Type nll_oul(vector<Type> mut, vector<Type> mutm, Type dt, vector<Type> beta, vector<Type> gamma, vector<Type> varState){

  vector<Type> state(2);
  matrix<Type> cov(2,2);
  cov.setZero();
  cov(0,0) = varState(0);
  cov(1,1) = varState(1);
  // cov(1,0) = rho * sqrt(varState(0) * varState(1));
  // cov(0,1) = rho * sqrt(varState(0) * varState(1));
  
  matrix<Type> B(2,2);
  B.setZero();
  B(0,0) = beta(0);
  B(1,0) = beta(1);
  B(0,1) = beta(2);
  B(1,1) = beta(3);

  // matrix<Type> Bks = kroneckersum(B,B);
  // matrix<Type> I(Bks.cols(),Bks.cols());
  // I.setIdentity();
  
  matrix<Type> meb = expm((matrix<Type>)(-B * dt));
  // matrix<Type> meBks = expm((matrix<Type>)(Bks * dt));

  state = mut - (gamma + (vector<Type>)(meb * (mutm - gamma).matrix()));

  matrix<Type> Bks = convenience::kroneckersum(B,B);
  
  vector<Type> varVec = (matrix<Type>)Bks.inverse() * cov.vec().matrix();
  matrix<Type> varMat = asMatrix(varVec,2,2);
 

  // matrix<Type> covVec(4,1);
  // covVec = cov.vec();
  // vector<Type> varVec = ((matrix<Type>)Bks.inverse()) * (matrix<Type>)((I - meBks) * covVec);
  // matrix<Type> var = asMatrix(varVec,2,2);

  matrix<Type> var = varMat - (matrix<Type>)(meb * (matrix<Type>)(varMat * (matrix<Type>)meb.transpose()));
  
  return density::MVNORM<Type>(var)(state);
}



#endif

