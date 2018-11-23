// Save this file as movement.cpp
// Copied and modified from argosTrack: http://github.com/calbertsen/argosTrack

#include <TMB.hpp>
#include <argosTrack.hpp>
#include <covafill/TMB>



using namespace density;

using namespace argosTrack;

template<class Type>
Type objective_function<Type>::operator() ()
{
  //////////
  // DATA //
  //////////
  
  // Observation related
  DATA_VECTOR(lon);
  DATA_VECTOR(lat);
  DATA_VECTOR(dayOfYear);
  DATA_VECTOR(include);
  DATA_FACTOR(qual);
  DATA_IVECTOR(varModelCode);

  // Movement related
  DATA_VECTOR(dtStates);
  DATA_INTEGER(moveModelCode);
  DATA_INTEGER(nauticalStates);

  // Measurement related
  DATA_SCALAR(minDf);
  DATA_INTEGER(errorModelCode);
  DATA_INTEGER(nauticalObs);
  DATA_VECTOR(splineKnots);

  // Related to more than one
  DATA_IVECTOR(prevState);
  DATA_VECTOR(stateFrac);


  // Residual related  
  DATA_VECTOR_INDICATOR(klon,lon);
  DATA_VECTOR_INDICATOR(klat,lat);


  ////////////////
  // PARAMETERS //
  ////////////////

  // Movement related
  PARAMETER_VECTOR(movePars); //Length 2 (first lat then lon) x number of states
  PARAMETER_VECTOR(logSdState);

  // States
  PARAMETER_MATRIX(mu);
  PARAMETER_MATRIX(vel);


  // Measurement related  
  PARAMETER_VECTOR(logSdObs);
  PARAMETER_VECTOR(logSdObsExtra);
  PARAMETER_MATRIX(logCorrection);
  PARAMETER(splineXlogSd);
  PARAMETER_VECTOR(knotPars);
  PARAMETER_VECTOR(df);  //Length as number of quality classes


  // SPline stuff
  DATA_MATRIX(knots_mu);
 
  DATA_VECTOR(rep_lat);
  DATA_VECTOR(rep_lon);
   
  PARAMETER_VECTOR(knotpar_mu_lat);
  PARAMETER_VECTOR(knotpar_mu_lon);

  PARAMETER(logLambdaLat);
  PARAMETER(logLambdaLon);
  
  DATA_INTEGER(p);
  DATA_VECTOR(h);
  DATA_SCALAR(d);
  covafill<Type> cfLat(knots_mu,knotpar_mu_lat,h,p);
  covafill<Type> cfLon(knots_mu,knotpar_mu_lon,h,p);
  covatree<Type> ctLat(d, &cfLat);
  covatree<Type> ctLon(d, &cfLon);
  
  vector<Type> muUse(2);
  muUse.setZero();

  
  // Get state coordinates in latitude/longitude
  vector<Type> slon(mu.cols());
  vector<Type> slat(mu.cols());

  for(int i = 0; i < mu.cols(); ++i){
    slon(i) = mu(1,i);
    slat(i) = mu(0,i);
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

  for(int i = 0; i < knotpar_mu_lat.size(); ++i)
    nll -= dnorm(knotpar_mu_lat(i),Type(0.0),exp(logLambdaLat),true);
  for(int i = 0; i < knotpar_mu_lon.size(); ++i)
    nll -= dnorm(knotpar_mu_lon(i),Type(0.0),exp(logLambdaLon),true);
  
  ////////////////////////////////
  // Create covariance matrices //
  ////////////////////////////////
  
  //Set up covariance matrix for observations
  // Observational distributions
  vector<densities::MVT_tt<Type> > nll_dist_obs(varObs.cols());
  matrix<Type> covObs(2,2);
  covObs.setZero();
  covObs(0,0) = 1.0;
  covObs(1,1) = 1.0;
  vector<Type> obs(2);

  // For argos data
  for(int i = 0; i < nll_dist_obs.size(); ++i){
    covObs.setZero();
    covObs(0,0) = varObs(0,i);
    covObs(1,1) = varObs(1,i);
    covObs(1,0) = 0.0; 
    covObs(0,1) = covObs(1,0);
    //ModelCode: 0: t; 1: norm; 2: symmetric hyperbolic
    nll_dist_obs(i) = densities::MVT_tt<Type>(covObs,exp(df(i))+minDf,errorModelCode);
  }

  
  
  ///////////////////////////////////
  // Contribution from first state //
  ///////////////////////////////////

  
  ///////////////////////////////
  // Contributions from states //
  ///////////////////////////////

  using namespace movement;
  
  for(int i = 1; i < mu.cols(); ++i){
    // case 6:			// Irregularized Discrete time correlated random walk on lat+lon
    muUse(0) = ctLat((vector<Type>)mu.col(i-1))(0);
    muUse(1) = ctLon((vector<Type>)mu.col(i-1))(0);
    if(i == 1){
	nll += nll_idtcrw1((vector<Type>)mu.col(i),
			   (vector<Type>)mu.col(i-1),
			   dtStates(i),
			   (vector<Type>)(Type(1.0)/(Type(1.0)+exp(-movePars.segment(0,2)))),
			   movePars(2),
			   Type(2.0)/(Type(1.0)+exp(-movePars(3))) - Type(1.0),
			   muUse,
			   varState);
      }else{
	nll += nll_idtcrw((vector<Type>)mu.col(i),
			 (vector<Type>)mu.col(i-1),
			 (vector<Type>)mu.col(i-2),
			  dtStates(i),
			  dtStates(i-1),
			  (vector<Type>)(Type(1.0)/(Type(1.0)+exp(-movePars.segment(0,2)))),
			  movePars(2),
			  Type(2.0)/(Type(1.0)+exp(-movePars(3))) - Type(1.0),
			  muUse,
			  varState);
      }
  }

  /////////////////////////////////////
  // Contributions from observations //
  /////////////////////////////////////
  
  for(int i = 0; i < lon.size(); ++i){
    obs.setZero();
    obs(0) = lat(i);
    obs(1) = lon(i);
    if(prevState(i)+1 >= slat.size()){
      obs(0) -= slat(prevState(i));
      obs(1) -= slon(prevState(i));
    }else{
      obs(0) -= stateFrac(i) * slat(prevState(i)) +
	(Type(1.0) - stateFrac(i)) * slat(prevState(i)+1);
      obs(1) -= stateFrac(i) * slon(prevState(i)) +
	(Type(1.0) - stateFrac(i)) * slon(prevState(i)+1);
    }
      nll += nll_dist_obs(qual(i))(obs)*include(i)*klon(i)*klat(i);
  }

  matrix<Type> muLonOut(rep_lat.size(),rep_lon.size());
  for(int i = 0; i < rep_lat.size(); ++i)
    for(int j = 0; j < rep_lon.size(); ++j){
      vector<Type> tmp(2);
      tmp(0) = rep_lat(i); tmp(1) = rep_lon(j);
      muLonOut(i,j) = cfLon(tmp)(0);
    }
  REPORT(muLonOut);
  //ADREPORT(muLonOut);
  // }

  //if(isDouble<Type>::value){
  matrix<Type> muLatOut(rep_lat.size(),rep_lon.size());
  for(int i = 0; i < rep_lat.size(); ++i)
    for(int j = 0; j < rep_lon.size(); ++j){
      vector<Type> tmp(2);
      tmp(0) = rep_lat(i); tmp(1) = rep_lon(j);
      muLatOut(i,j) = cfLat(tmp)(0);
    }
  REPORT(muLatOut);
 
  
  ////////////
  // REPORT //
  ////////////
  
  REPORT(slat);
  REPORT(slon);

  //////////////
  // ADREPORT //
  //////////////
  
  vector<Type> dfs = exp(df)+minDf;
  ADREPORT(correction);
  ADREPORT(sdObs);
  ADREPORT(dfs);
  
  return nll;
  
}

