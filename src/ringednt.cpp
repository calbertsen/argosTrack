#include <TMB.hpp>
#include "densities/all.hpp"
#include "movement/models.hpp"

using namespace density;

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
  DATA_INTEGER(timevary);

  DATA_VECTOR_INDICATOR(klon,lon);
  DATA_VECTOR_INDICATOR(klat,lat);
  
  PARAMETER_MATRIX(logbeta); //Length 2 (first lat then lon) x number of states
  PARAMETER_VECTOR(logSdbeta);
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

  matrix<Type> beta = logbeta.array().exp().matrix();

  if(moveModelCode == 2){
    beta.row(2) += beta.row(0);
    beta.row(3) += beta.row(1);
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
      case 0: 			// Random walk on lat+lon
	nll += nll_rw((vector<Type>)mu.col(stateNum),
			 (vector<Type>)mu.col(stateNum-1),
			 dt(i),varState);
	break;
      case 1:			// Continuous Time Correlated Random Walk on lat+lon
	nll += nll_ctcrw((vector<Type>)mu.col(stateNum),
			 (vector<Type>)mu.col(stateNum-1),
			 (vector<Type>)vel.col(stateNum),
			 (vector<Type>)vel.col(stateNum-1),
			 dt(i),
			 (vector<Type>)beta.col(stateNum),
			 gamma,varState);
	break;
      case 2:			// Mixed memory Continuous Time Correlated Random Walk on lat+lon
	nll += nll_mmctcrw((vector<Type>)mu.col(stateNum),
			   (vector<Type>)mu.col(stateNum-1),
			   (vector<Type>)vel.col(stateNum),
			   (vector<Type>)vel.col(stateNum-1),
			   dt(i),
			   (vector<Type>)beta.col(stateNum),
			   gamma,varState);
	break;
      case 3:			// Discrete time correlated random walk on lat+lon
	nll += Type(0.0); // dtcrw
      case 4:		 // Discrete steplength + bearings model
	nll += Type(0.0); //dsb
      default:
	error("Movement model not implemented");
	break;
      }

      /*if(timevary == 1){
	nll -= dnorm(logbeta(0,stateNum),logbeta(0,stateNum-1),sqrt(dt(i))*exp(logSdbeta(0)),true);
	nll -= dnorm(logbeta(1,stateNum),logbeta(1,stateNum-1),sqrt(dt(i))*exp(logSdbeta(1)),true);
	if(moveModelCode == 2){
	  nll -= dnorm(logbeta(2,stateNum),logbeta(2,stateNum-1),sqrt(dt(i))*exp(logSdbeta(2)),true);
	  nll -= dnorm(logbeta(3,stateNum),logbeta(3,stateNum-1),sqrt(dt(i))*exp(logSdbeta(3)),true);
	}
	}*/
          
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
    nll += nll_dist_obs(qual(i))(obs)*include(i)*klon(i)*klat(i);

  }
  vector<Type> dfs = exp(df)+minDf;
  ADREPORT(correction);
  ADREPORT(sdObs);
  ADREPORT(dfs);
  ADREPORT(beta);
  return nll;
  
}

