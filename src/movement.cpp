#include <TMB.hpp>
#include "convenience/convenience.hpp"
#include "densities/all.hpp"
#include "movement/models.hpp"

using namespace density;


template<class Type>
Type objective_function<Type>::operator() ()
{

  DATA_VECTOR(lon);
  DATA_VECTOR(lat);
  DATA_VECTOR(dtStates);
  DATA_IVECTOR(prevState);
  DATA_VECTOR(stateFrac);
  DATA_FACTOR(qual); //Integers
  DATA_VECTOR(include);
  DATA_SCALAR(minDf);
  DATA_INTEGER(moveModelCode);
  DATA_INTEGER(modelCode);
  DATA_INTEGER(nauticalStates);
  DATA_INTEGER(nauticalObs);
  DATA_INTEGER(timevary);

  DATA_VECTOR_INDICATOR(klon,lon);
  DATA_VECTOR_INDICATOR(klat,lat);
  
  PARAMETER_MATRIX(logbeta); //Length 2 (first lat then lon) x number of states
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


  // Get state coordinates in nautical miles
  vector<Type> x(mu.cols());
  vector<Type> y(mu.cols());
  // Get state coordinates in latitude/longitude
  vector<Type> slon(mu.cols());
  vector<Type> slat(mu.cols());

  if(nauticalStates){
    for(int i = 0; i < x.size(); ++i){
      vector<Type> tmp = n2ll(mu(1,i), mu(0,i));
      x(i) = mu(1,i);
      y(i) = mu(0,i);
      slon(i) = tmp(0);
      slat(i) = tmp(1);
    }
  }else{
    for(int i = 0; i < x.size(); ++i){
      vector<Type> tmp = ll2n(mu(1,i), mu(0,i));
      x(i) = tmp(0);
      y(i) = tmp(1);
      slon(i) = mu(1,i);
      slat(i) = mu(0,i);
    }
  }

  // Get observation coordinates in nautical miles
  vector<Type> xobs(lon.size());
  vector<Type> yobs(lat.size());

  for(int i = 0; i < xobs.size(); ++i){
    vector<Type> tmp = ll2n(lon(i), lat(i));
    xobs(i) = tmp(0);
    yobs(i) = tmp(1);
  }

  // Get state steplength and bearing
  vector<Type> stepLengths(x.size());
  vector<Type> bearings(x.size());
  stepLengths.setZero();
  bearings.setZero();

  for(int i = 1; i < stepLengths.size(); ++i){
    stepLengths(i) = stepLength(x(i-1),y(i-1),x(i),y(i),true);
    bearings(i) = bearing(x(i-1),y(i-1),x(i),y(i),true);
  }


  // Calculate beta values
  matrix<Type> beta = logbeta.array().exp().matrix();
  if(moveModelCode == 2){
    beta.row(2) += beta.row(0);
    beta.row(3) += beta.row(1);
  }

  // Transform parameters
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

  // Variable for negative log-likelihood
  Type nll = 0.0;


  // Observational distributions
  vector<MVT_tt<Type> > nll_dist_obs(varObs.cols());
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

  // Contribution from first state
  
  // Contributions from states
  for(int i = 1; i < mu.cols(); ++i){
    switch(moveModelCode){
    case 0: 			// Random walk on lat+lon
      nll += nll_rw((vector<Type>)mu.col(i),
		    (vector<Type>)mu.col(i-1),
		    dtStates(i),varState);
      break;
    case 1:			// Continuous Time Correlated Random Walk on lat+lon
      nll += nll_ctcrw((vector<Type>)mu.col(i),
		       (vector<Type>)mu.col(i-1),
		       (vector<Type>)vel.col(i),
		       (vector<Type>)vel.col(i-1),
		       dtStates(i),
		       (vector<Type>)beta.col(i),
		       gamma,varState);
      break;
    case 2:			// Mixed memory Continuous Time Correlated Random Walk on lat+lon
      nll += nll_mpctcrw((vector<Type>)mu.col(i),
			 (vector<Type>)mu.col(i-1),
			 (vector<Type>)vel.col(i),
			 (vector<Type>)vel.col(i-1),
			 dtStates(i),
			 (vector<Type>)beta.col(i),
			 gamma,varState);
      break;
    case 3:			// Discrete time correlated random walk on lat+lon
      if(i == 1){
	nll += nll_dtcrw1((vector<Type>)mu.col(i),
			 (vector<Type>)mu.col(i-1),
			 Type(2.0)/(Type(1.0)+exp(-logbeta(0,i))) - Type(1.0),
			 gamma(0),
			 Type(2.0)/(Type(1.0)+exp(-gamma(1))) - Type(1.0),
			 varState);
      }else{
	nll += nll_dtcrw((vector<Type>)mu.col(i),
			 (vector<Type>)mu.col(i-1),
			 (vector<Type>)mu.col(i-2),
			 Type(2.0)/(Type(1.0)+exp(-logbeta(0,i))) - Type(1.0),
			 gamma(0),
			 Type(2.0)/(Type(1.0)+exp(-gamma(1))) - Type(1.0),
			 varState);
      }
      break;
    case 4:		 // Discrete steplength + bearings model
      nll += nll_dsb(stepLengths(i),
		      bearings(i),
		      bearings(i-1),
		      beta(0,i),
		      varState(0),
		      varState(1));
      break;
    default:
      error("Movement model not implemented");
      break;
    }
  }


  // Contributions from observations
  for(int i = 0; i < lon.size(); ++i){
    obs.setZero();
    if(nauticalObs){
      obs(0) = yobs(i);
      obs(1) = xobs(i);
      if(stateFrac(i)+1 > y.size()){
	obs(0) -= y(prevState(i));
	obs(1) -= x(prevState(i));
      }else{
	obs(0) -= stateFrac(i) * y(prevState(i)) + (Type(1.0) - stateFrac(i)) * y(prevState(i)+1);
	obs(1) -= stateFrac(i) * x(prevState(i)) + (Type(1.0) - stateFrac(i)) * x(prevState(i)+1);
      }
    }else{
      obs(0) = lat(i);
      obs(1) = lon(i);
      if(stateFrac(i)+1 > slat.size()){
	obs(0) -= slat(prevState(i));
	obs(1) -= slon(prevState(i));
      }else{
	obs(0) -= stateFrac(i) * slat(prevState(i)) + (Type(1.0) - stateFrac(i)) * slat(prevState(i)+1);
	obs(1) -= stateFrac(i) * slon(prevState(i)) + (Type(1.0) - stateFrac(i)) * slon(prevState(i)+1);
      }
    }
    nll += nll_dist_obs(qual(i))(obs)*include(i)*klon(i)*klat(i);
  }


  REPORT(x);
  REPORT(y);
  REPORT(slat);
  REPORT(slon);
  REPORT(stepLengths);
  REPORT(bearings);
  REPORT(xobs);
  REPORT(yobs);
  
  vector<Type> dfs = exp(df)+minDf;
  ADREPORT(correction);
  ADREPORT(sdObs);
  ADREPORT(dfs);
  ADREPORT(beta);

  // ADREPORT(slat);
  // ADREPORT(slon);

  
  return nll;
  
  }

