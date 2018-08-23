#include <TMB.hpp>
#include "convenience/convenience.hpp"
#include "densities/all.hpp"
#include "movement/models.hpp"

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
  PARAMETER(logSdObsExtra);
  PARAMETER_MATRIX(logCorrection);
  PARAMETER(splineXlogSd);
  PARAMETER_VECTOR(knotPars);
  PARAMETER_VECTOR(df);  //Length as number of quality classes





  ///////////////////////////////////////////
  // Transform to nautical miles / lat/lon //
  ///////////////////////////////////////////
  

  // Get state coordinates in nautical miles
  vector<Type> x(mu.cols());
  vector<Type> y(mu.cols());
  // Get state coordinates in latitude/longitude
  vector<Type> slon(mu.cols());
  vector<Type> slat(mu.cols());

  if(nauticalStates){
    for(int i = 0; i < x.size(); ++i){
      vector<Type> tmp = convenience::n2ll(mu(1,i), mu(0,i));
      x(i) = mu(1,i);
      y(i) = mu(0,i);
      slon(i) = tmp(0);
      slat(i) = tmp(1);
    }
  }else{
    for(int i = 0; i < x.size(); ++i){
      vector<Type> tmp = convenience::ll2n(mu(1,i), mu(0,i));
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
    vector<Type> tmp = convenience::ll2n(lon(i), lat(i));
    xobs(i) = tmp(0);
    yobs(i) = tmp(1);
  }

  // Get state steplength and bearing
  vector<Type> stepLengths(x.size());
  vector<Type> bearings(x.size());
  stepLengths.setZero();
  bearings.setZero();

  for(int i = 1; i < stepLengths.size(); ++i){
    stepLengths(i) = convenience::stepLength(x(i-1),y(i-1),x(i),y(i),true);
    bearings(i) = convenience::bearing(x(i-1),y(i-1),x(i),y(i),true);
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

  // for known positions
  matrix<Type> covKnown(2,2);
  covKnown.setZero(); covKnown(0,0) = Type(0.00001); covKnown(1,1) = Type(0.00001);
  densities::MVT_tt<Type> nll_dist_known(covKnown,Type(10000.0), 1); // Normal distribution
  // For geolocation
  // Create spline
  tmbutils::splinefun<Type> logSplLat(splineKnots,knotPars);
  densities::MVT_tt<Type> nll_dist_geoloc(covObs,exp(df(0))+minDf,errorModelCode);

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
		       exp((vector<Type>)movePars.segment(0,2)),
		       (vector<Type>)movePars.segment(2,2),
		       varState);
      break;
    case 2:			// Mixed memory Continuous Time Correlated Random Walk on lat+lon
      // nll += nll_mpctcrw((vector<Type>)mu.col(i),
      // 			 (vector<Type>)mu.col(i-1),
      // 			 (vector<Type>)vel.col(i),
      // 			 (vector<Type>)vel.col(i-1),
      // 			 dtStates(i),
      // 			 exp((vector<Type>)movePars.segment(0,4)),
      // 			 (vector<Type>)movePars.segment(4,4),
      // 			 varState);
      break;
    case 3:			// Discrete time correlated random walk on lat+lon
      if(i == 1){
	nll += nll_dtcrw1((vector<Type>)mu.col(i),
			 (vector<Type>)mu.col(i-1),
			 Type(1.0)/(Type(1.0)+exp(-movePars(0))),
			 movePars(1),
			 Type(2.0)/(Type(1.0)+exp(-movePars(2))) - Type(1.0),
			 varState);
      }else{
	nll += nll_dtcrw((vector<Type>)mu.col(i),
			 (vector<Type>)mu.col(i-1),
			 (vector<Type>)mu.col(i-2),
			 Type(1.0)/(Type(1.0)+exp(-movePars(0))),
			 movePars(1),
			 Type(2.0)/(Type(1.0)+exp(-movePars(2))) - Type(1.0),
			 varState);
      }
      break;
    case 4:		 // Discrete steplength + bearings model
      nll += nll_dsb_weibull(stepLengths(i),
			     bearings(i),
			     bearings(i-1),
			     exp(movePars(0)),
			     exp(movePars(1)),
			     exp(movePars(2)));
      break;
    case 5:		 // Discrete steplength + bearings model
      nll += nll_dsb_halfnorm(stepLengths(i),
			      bearings(i),
			      bearings(i-1),
			      exp(movePars(0)),
			      varState(0));
      break;      
    case 6:			// Irregularized Discrete time correlated random walk on lat+lon
      if(i == 1){
	nll += nll_idtcrw1((vector<Type>)mu.col(i),
			   (vector<Type>)mu.col(i-1),
			   dtStates(i),
			   (vector<Type>)(Type(1.0)/(Type(1.0)+exp(-movePars.segment(0,2)))),
			   movePars(2), //Type(2.0*M_PI)/(Type(1.0)+exp(-movePars(1))),
			   Type(2.0)/(Type(1.0)+exp(-movePars(3))) - Type(1.0),
			   (vector<Type>)movePars.segment(4,2),
			   varState);
      }else{
	nll += nll_idtcrw((vector<Type>)mu.col(i),
			 (vector<Type>)mu.col(i-1),
			 (vector<Type>)mu.col(i-2),
			  dtStates(i),
			  dtStates(i-1),
			  (vector<Type>)(Type(1.0)/(Type(1.0)+exp(-movePars.segment(0,2)))),
			  movePars(2), //Type(2.0*M_PI)/(Type(1.0)+exp(-movePars(1))),
			  Type(2.0)/(Type(1.0)+exp(-movePars(3))) - Type(1.0),
			  (vector<Type>)movePars.segment(4,2),
			  varState);
      }
      break;
    case 7: 			// Ornstein-Uhlenbeck Location
      nll += nll_oul((vector<Type>)mu.col(i),
      		       (vector<Type>)mu.col(i-1),
      		       dtStates(i),
      		       (vector<Type>)movePars.segment(0,4),
      		       (vector<Type>)movePars.segment(4,2),
      		       varState);
      break;
    case 8:			// Ornstein-Uhlenbeck Velocity
      // if(i == 1){
      // 	nll += nll_ouv1((vector<Type>)mu.col(i),
      // 			   (vector<Type>)mu.col(i-1),
      // 			   dtStates(i),
      // 			   (vector<Type>)(Type(1.0)/(Type(1.0)+exp(-movePars.segment(0,4)))),
      // 			   Type(2.0)/(Type(1.0)+exp(-movePars(4))) - Type(1.0),
      // 			   (vector<Type>)movePars.segment(5,2),
      // 			   varState);
      // }else{
      // 	nll += nll_ouv((vector<Type>)mu.col(i),
      // 		       (vector<Type>)mu.col(i-1),
      // 		       (vector<Type>)mu.col(i-2),
      // 		       dtStates(i),
      // 		       dtStates(i-1),
      // 		       (vector<Type>)(Type(1.0)/(Type(1.0)+exp(-movePars.segment(0,4)))),
      // 		       Type(2.0)/(Type(1.0)+exp(-movePars(4))) - Type(1.0),
      // 		       (vector<Type>)movePars.segment(5,2),
      // 		       varState);
      // }
      break;
    case 9:
      // nll += nll_csb(stepLengths(i),
      // 		     stepLengths(i-1),
      // 		     bearings(i),
      // 		     bearings(i-1),
      // 		     exp(movePars(0)),
      // 		     exp(movePars(1)),
      // 		     sqrt(varState(0)),
      // 		     sqrt(varState(1)),
      // 		     dtStates(i));
      break;
    case 10:
      
      break;
    default:
      error("Movement model not implemented");
      break;
    }
  }

  /////////////////////////////////////
  // Contributions from observations //
  /////////////////////////////////////
  
  for(int i = 0; i < lon.size(); ++i){
    obs.setZero();
    if(nauticalObs){
      obs(0) = yobs(i);
      obs(1) = xobs(i);
      if(prevState(i)+1 >= y.size()){
	obs(0) -= y(prevState(i));
	obs(1) -= x(prevState(i));
      }else{
	obs(0) -= stateFrac(i) * y(prevState(i)) +
	  (Type(1.0) - stateFrac(i)) * y(prevState(i)+1);
	obs(1) -= stateFrac(i) * x(prevState(i)) +
	  (Type(1.0) - stateFrac(i)) * x(prevState(i)+1);
      }
    }else{
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
    }

    switch(varModelCode(i)){
    case 0: 			// Argos data with location class GPS,3,2,1,0,A,B,Z
      nll += nll_dist_obs(qual(i))(obs)*include(i)*klon(i)*klat(i);
      break;
    case 1:			// "known" position
      nll += nll_dist_known(obs)*include(i)*klon(i)*klat(i);
      break;
    case 2:			// Geolocation data
      nll_dist_geoloc.setSigma(convenience::geolocVarMat(exp(splineXlogSd), dayOfYear(i), logSplLat));
      nll += nll_dist_geoloc(obs)*include(i)*klon(i)*klat(i);
    case 3:
      nll_dist_geoloc.setSigma(convenience::geolocVarMatFormula(exp(logSdObs(0)), dayOfYear(i), exp(logSdObs(1)), logSdObsExtra));
      nll += nll_dist_geoloc(obs)*include(i)*klon(i)*klat(i);
      break;
    }
  }


  ///////////////////////////////////
  // Calculate spline for each day //
  ///////////////////////////////////
  
  vector<Type> splineSd(366);
  for(int i = 0; i < splineSd.size(); ++i)
    splineSd(i) = exp(logSplLat(i+1));

  ////////////
  // REPORT //
  ////////////
  
  REPORT(x);
  REPORT(y);
  REPORT(slat);
  REPORT(slon);
  REPORT(stepLengths);
  REPORT(bearings);
  REPORT(xobs);
  REPORT(yobs);
  REPORT(splineSd);

  //////////////
  // ADREPORT //
  //////////////
  
  vector<Type> dfs = exp(df)+minDf;
  ADREPORT(correction);
  ADREPORT(sdObs);
  ADREPORT(dfs);
  ADREPORT(splineSd);

  // ADREPORT(slat);
  // ADREPORT(slon);

  
  return nll;
  
}

