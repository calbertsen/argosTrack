#include <TMB.hpp>

using namespace density;



/** \brief Multivariate t distribution with user supplied scale matrix

Class to evaluate the negative log density of a multivariate t distributed variable with general scale matrix Sigma and location vector 0 and df degrees of freedom.
*/
template <class Type>
class MVT_tt: public MVNORM_t<Type>
{
  Type df;
  bool useNorm;

public:
  MVT_tt()
    : MVNORM_t<Type>()
  {
    useNorm = false;
  };
  MVT_tt(Type df_)
    : MVNORM_t<Type>()
  {
    df = df_;
    useNorm = false;
  }
  MVT_tt(matrix<Type> Sigma_, Type df_)
    : MVNORM_t<Type>(Sigma_)
  {
    df = df_;
    useNorm = false;
  }
  MVT_tt(matrix<Type> Sigma_, Type df_, bool useNorm_)
    : MVNORM_t<Type>(Sigma_)
  {
    df = df_;
    useNorm = useNorm_;
  }

  void setdf(Type df_){
    df = df_;
  }

  /** \brief Evaluate the negative log density */
  Type operator()(vector<Type> x){
    Type p = x.size();
    //Lange et al. 1989 http://www.jstor.org/stable/2290063
    Type tdens = -lgamma(Type(0.5)*(df+p))+lgamma(Type(0.5)*df)+p*Type(0.5)*log(df)+p*lgamma(Type(0.5))-Type(0.5)*this->logdetQ + Type(0.5)*(df+p)*log(Type(1.0)+this->Quadform(x)/df);
    Type ndens = -Type(.5)*this->logdetQ + Type(.5)*this->Quadform(x) + p*Type(log(sqrt(2.0*M_PI)));

    if(useNorm) return ndens; else return tdens;
  }
};





// Define movement model functions

// Continuous-time correlated random walk
template<class Type>
Type nll_ctcrw(vector<Type> mut, vector<Type> mutm, vector<Type> velt, vector<Type> veltm,Type dt, vector<Type> beta, vector<Type> gamma, vector<Type> varState){

  vector<Type> state(4);
  matrix<Type> cov(4,4);
  cov.setZero();

  state(0) = mut(0)-(mutm(0)+veltm(0)*(1.0-exp(-beta(0)*dt))/beta(0));
  state(1) = velt(0) - (gamma(0)+exp(-beta(0)*dt)*(veltm(0)-gamma(0)));

  state(2) = mut(1)-(mutm(1)+veltm(1)*(1.0-exp(-beta(1)*dt))/beta(1));
  state(3) = velt(1) - (gamma(1)+exp(-beta(1)*dt)*(veltm(1)-gamma(1)));

  cov(0,0) = varState(0)/pow(beta(0),2.0)*(dt-2.0*(1.0-exp(-beta(0)*dt))/beta(0)+(1.0-exp(-2.0*beta(0)*dt))/(2.0*beta(0)));
  cov(1,1) = varState(0)*(1.0-exp(-2.0*beta(0)*dt))/(2*beta(0));
  cov(1,0) = varState(0)*(1.0-2.0*exp(-beta(0)*dt)+exp(-2.0*beta(0)*dt))/(2.0*pow(beta(0),2.0));
  cov(0,1) = cov(1,0);
      
  cov(2,2) = varState(1)/pow(beta(1),2.0)*(dt-2.0*(1.0-exp(-beta(1)*dt))/beta(1)+(1.0-exp(-2.0*beta(1)*dt))/(2.0*beta(1)));
  cov(3,3) = varState(1)*(1.0-exp(-2.0*beta(1)*dt))/(2.0*beta(1));
  cov(2,3) = varState(1)*(1.0-2.0*exp(-beta(1)*dt)+exp(-2.0*beta(1)*dt))/(2.0*pow(beta(1),2.0));
  cov(3,2) = cov(2,3);
	
  return MVNORM<Type>(cov)(state);
}

// Mixed memory continuous-time correlated random walk
template<class Type>
Type nll_mmctcrw(vector<Type> mut, vector<Type> mutm, vector<Type> velt, vector<Type> veltm,Type dt, vector<Type> beta, vector<Type> gamma, vector<Type> varState){

  vector<Type> state(6);
  matrix<Type> cov(6,6);
  cov.setZero();

  state(0) = mut(0)-(mutm(0)+veltm(0)*(1.0-exp(-beta(0)*dt))/beta(0)+veltm(2)*(1.0-exp(-beta(2)*dt))/beta(2));
  state(1) = velt(0) - (gamma(0)+exp(-beta(0)*dt)*(veltm(0)-gamma(0)));
  state(2) = velt(2) - (gamma(2)+exp(-beta(2)*dt)*(veltm(2)-gamma(2)));

  state(3) = mut(1)-(mutm(1)+veltm(1)*(1.0-exp(-beta(1)*dt))/beta(1)+veltm(3)*(1.0-exp(-beta(3)*dt))/beta(3));
  state(4) = velt(1) - (gamma(1)+exp(-beta(1)*dt)*(veltm(1)-gamma(1)));
  state(5) = velt(3) - (gamma(3)+exp(-beta(3)*dt)*(veltm(3)-gamma(3)));

  cov(0,0) = varState(0)/pow(beta(0),2.0)*(dt-2.0*(1.0-exp(-beta(0)*dt))/beta(0)+(1.0-exp(-2.0*beta(0)*dt))/(2.0*beta(0)));
  cov(0,0) += varState(2)/pow(beta(2),2.0)*(dt-2.0*(1.0-exp(-beta(2)*dt))/beta(2)+(1.0-exp(-2.0*beta(2)*dt))/(2.0*beta(2)));
  cov(0,0) += dt*varState(4);
  cov(1,1) = varState(0)*(1.0-exp(-2.0*beta(0)*dt))/(2*beta(0));
  cov(2,2) = varState(2)*(1.0-exp(-2.0*beta(2)*dt))/(2*beta(2));
  cov(1,0) = varState(0)*(1.0-2.0*exp(-beta(0)*dt)+exp(-2.0*beta(0)*dt))/(2.0*pow(beta(0),2.0));
  cov(0,1) = cov(1,0);
  cov(2,0) = varState(2)*(1.0-2.0*exp(-beta(2)*dt)+exp(-2.0*beta(2)*dt))/(2.0*pow(beta(2),2.0));
  cov(0,2) = cov(2,0);
  cov(2,1) = 0.0;
  cov(1,2) = cov(2,1);
      
  cov(3,3) = varState(1)/pow(beta(1),2.0)*(dt-2.0*(1.0-exp(-beta(1)*dt))/beta(1)+(1.0-exp(-2.0*beta(1)*dt))/(2.0*beta(1)));
  cov(3,3) += varState(3)/pow(beta(3),2.0)*(dt-2.0*(1.0-exp(-beta(3)*dt))/beta(3)+(1.0-exp(-2.0*beta(3)*dt))/(2.0*beta(3)));
  cov(3,3) += dt*varState(5);
  cov(4,4) = varState(1)*(1.0-exp(-2.0*beta(1)*dt))/(2.0*beta(1));
  cov(5,5) = varState(3)*(1.0-exp(-2.0*beta(3)*dt))/(2.0*beta(3));
  cov(3,4) = varState(1)*(1.0-2.0*exp(-beta(1)*dt)+exp(-2.0*beta(1)*dt))/(2.0*pow(beta(1),2.0));
  cov(4,3) = cov(3,4);
  cov(3,5) = varState(3)*(1.0-2.0*exp(-beta(3)*dt)+exp(-2.0*beta(3)*dt))/(2.0*pow(beta(3),2.0));
  cov(5,3) = cov(3,5);
  cov(4,5) = 0.0;
  cov(5,4) = cov(4,5);
	
  return MVNORM<Type>(cov)(state);
}

// Random walk
template<class Type>
Type nll_rw(vector<Type> mut, vector<Type> mutm, Type dt, vector<Type> varState){

  vector<Type> state(2);
  matrix<Type> cov(2,2);

  state = mut-mutm;
  cov.diagonal() = varState*sqrt(dt);
	
  return MVNORM<Type>(cov)(state);
}


// (Irregularized version of) Discrete Time Correlated Random Walk
template<class Type>
Type nll_dtcrw(){

  return(Type(0.0));
}












template<class Type>
Type objective_function<Type>::operator() ()
{

  DATA_VECTOR(lon);
  DATA_VECTOR(lat);
  DATA_VECTOR(dt);
  DATA_FACTOR(qual); //Integers
  DATA_VECTOR(include);
  DATA_SCALAR(minDf);
  DATA_INTEGER(moveModelCode);
  DATA_INTEGER(modelCode);
  PARAMETER_VECTOR(logbeta); //Length 2 (first lat then lon)
  PARAMETER_VECTOR(logSdState);
  PARAMETER_VECTOR(logSdObs); //length 2
  //DATA_MATRIX(logCorrection); //Dim 2 x number of quality classes (first should be log(1)
  PARAMETER_MATRIX(logCorrection);
  PARAMETER_VECTOR(gamma); //Length 2 (first lat then lon)
  
  PARAMETER_MATRIX(mu); // Dim 2 x lon.size()
  PARAMETER_MATRIX(vel); // Dim 2 x lon.size()

  PARAMETER_VECTOR(df);  //Length as number of quality classes

  // Number of data points to include
  PARAMETER(numdata);

  vector<Type> beta = exp(logbeta);

  if(moveModelCode == 2){
    beta(2) += beta(0);
    beta(3) += beta(1);
  }

  vector<Type> varState = exp(Type(2.0)*logSdState);
  matrix<Type> varObs(logCorrection.rows(),logCorrection.cols()+1);
  matrix<Type> correction = logCorrection.array().exp().matrix();
  for(int i = 0; i < varObs.rows(); ++i){
    varObs(i,0) = exp(2.0*(logSdObs(i)));
    for(int j = 1; j < varObs.cols(); ++j){
      varObs(i,j) = exp(2.0*(logSdObs(i)+logCorrection(i,j-1)));
    }
  }

  matrix<Type> sdObs = varObs.array().sqrt().matrix();

  Type nll = 0.0;

  MVNORM_t<Type> nll_dist;//(df(0));
  vector<MVT_tt<Type> > nll_dist_obs(varObs.cols());

  // matrix<Type> cov(4,4);
  // vector<Type> state(4);
  matrix<Type> covObs(2,2);
  vector<Type> obs(2);

  //Set up covariance matrix for observations
  for(int i = 0; i < nll_dist_obs.size(); ++i){
    covObs.setZero();
    covObs(0,0) = varObs(0,i);
    covObs(1,1) = varObs(1,i);
    covObs(1,0) = 0.0; 
    covObs(0,1) = covObs(1,0);
    //ModelCode: 0: t; 1: norm
    nll_dist_obs(i) = MVT_tt<Type>(covObs,exp(df(i))+minDf,modelCode);

  }


  int c = 0;
  
  

  //
  int stateNum = 0; 

  vector<Type> test(2);
  test.setZero();
  
  for(int i = 0; i < dt.size(); ++i){

    if(dt(i) > 0 && i > 0){stateNum += 1;}
    if(stateNum == 0){//Distribution for first state
      /*
      state.setZero();
      state(0) = mu(0,0)-lat(0);
      state(1) = vel(0,1);
      state(2) = mu(1,0)-lon(0);
      state(3) = vel(1,1);

      cov.setZero();
      cov(0,0) = 0.1;
      cov(1,1) = 0.1;
      cov(2,2) = 0.1;
      cov(3,3) = 0.1;

      nll_dist.setSigma(cov);
      nll += nll_dist(state);
      */
      //nll += -dnorm(vel(0,0),Type(0.0),Type(0.1),true);
      //nll += -dnorm(vel(1,0),Type(0.0),Type(0.1),true);
      // if(moveModelCode == 2){
      // 	nll += -dnorm(vel(2,0),Type(0.0),Type(1),true);
      // 	nll += -dnorm(vel(3,0),Type(0.0),Type(1),true);
      // }
    }else if(dt(i)>0){ //Only at first time step
      //First states

      switch(moveModelCode){
      case 0:
	nll += nll_rw((vector<Type>)mu.col(stateNum),
			 (vector<Type>)mu.col(stateNum-1),
			 dt(i),varState);
	break;
      case 1:
	nll += nll_ctcrw((vector<Type>)mu.col(stateNum),
			 (vector<Type>)mu.col(stateNum-1),
			 (vector<Type>)vel.col(stateNum),
			 (vector<Type>)vel.col(stateNum-1),
			 dt(i),beta,gamma,varState);
	break;
      case 2:
	nll += nll_mmctcrw((vector<Type>)mu.col(stateNum),
			   (vector<Type>)mu.col(stateNum-1),
			   (vector<Type>)vel.col(stateNum),
			   (vector<Type>)vel.col(stateNum-1),
			   dt(i),beta,gamma,varState);
	break;
      default:
	error("Movement model not implemented");
	break;
      }
      
    }else{ //Or nothing else happens
    }

    //Then observations
    //Set up observation vector 
    obs.setZero();
    obs(0) = lat(i)-mu(0,stateNum);
    obs(1) = lon(i)-mu(1,stateNum);
    
    //Set up covariance matrix
    /*covObs.setZero();
    covObs(0,0) = varObs(0,qual(i));
    covObs(1,1) = varObs(1,qual(i));
    covObs(1,0) = 0.0; 
    covObs(0,1) = covObs(1,0);
    

    nll_dist_obs.setSigma(covObs);
    */
    //if(include(i)==1){
    Type keep = CppAD::CondExpLt(Type(i), numdata, Type(1), Type(0));
    nll += nll_dist_obs(qual(i))(obs)*include(i)*keep;

    test(0) += CppAD::CondExpEq(Type(i),numdata,obs(0),Type(0));
    test(1) += CppAD::CondExpEq(Type(i),numdata,obs(1),Type(0));
	//}
  }
  vector<Type> dfs = exp(df)+minDf;
  ADREPORT(correction);
  ADREPORT(sdObs);
  ADREPORT(dfs);
  ADREPORT(test);
  ADREPORT(beta);
  return nll;
  
}
