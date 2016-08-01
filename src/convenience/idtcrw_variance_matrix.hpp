

#ifndef _IDTCRWVARMAT_
#define _IDTCRWVARMAT_

template<class Type>
vector<Type> idtcrwVarMat(Type dt , vector<Type> gamma, Type phi, Type rho, vector<Type> varState){


  matrix<Type> cov(2,2);
  cov(0,0) = varState(0);
  cov(1,1) = varState(1);
  cov(1,0) = rho * sqrt(varState(0) * varState(1));
  cov(0,1) = rho * sqrt(varState(0) * varState(1));

  matrix<Type> Gth(2,2);
  Gth << -log(gamma(0)), phi, -phi, -log(gamma(1));
  matrix<Type> Gks = kroneckersum(Gth,Gth);
  matrix<Type> I(Gks.cols(),Gks.cols());
  I.setIdentity();
  // tmbutils::matexp<Type,4> meGks(Gks);
  matrix<Type> meGks = expm((matrix<Type>)(-Gks * dt));
  matrix<Type> covVec(4,1);
  covVec = cov.vec();
  
  
  // vector<Type> var = ((matrix<Type>)Gks.inverse()) * (matrix<Type>)((I - meGks(-dt)) * covVec);

   vector<Type> var = ((matrix<Type>)Gks.inverse()) * (matrix<Type>)((I - meGks) * covVec);


  return var;
}


//Export to R
extern "C" {

  
  SEXP idtcrwVarMat(SEXP dt , SEXP gamma, SEXP phi, SEXP rho, SEXP varState){

    vector<double> res = idtcrwVarMat(asDouble(dt),
				      asVector<double>(gamma),
				      asDouble(phi),
				      asDouble(rho),
				      asVector<double>(varState));
    return(asSEXP(asMatrix(res,2,2)));   

  }

}

#endif
