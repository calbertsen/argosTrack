

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
  Gth.setZero();
  Gth(0,0) = -log(gamma(0));
  Gth(1,1) = -log(gamma(1));
  Gth(0,1) = phi;
  Gth(1,0) = -phi;
  matrix<Type> Gks = kroneckersum(Gth,Gth);

  vector<Type> varVec = (matrix<Type>)Gks.inverse() * cov.vec().matrix();
  matrix<Type> varMat = asMatrix(varVec,2,2);
  matrix<Type> meGth = expm((matrix<Type>)(-Gth * dt));
  matrix<Type> var = varMat - (matrix<Type>)(meGth * (matrix<Type>)(varMat * (matrix<Type>)meGth.transpose()));

  return var.vec();
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
