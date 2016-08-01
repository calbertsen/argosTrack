#include <Eigen/Cholesky>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
//#include "convenience/convert.hpp"

using namespace Eigen;


SEXP asSEXP(double x){
  SEXP y = PROTECT(allocVector(REALSXP,1));
  REAL(y)[0] = x;

  UNPROTECT(1);
  return y; 

}

SEXP asSEXP(VectorXd x){
  int n = x.size();
  SEXP y = PROTECT(allocVector(REALSXP,n));

  for(int i = 0; i < n; ++i)
    REAL(y)[i] = x[i];

  UNPROTECT(1);
  return y; 
}
SEXP asSEXP(VectorXi x){
  int n = x.size();
  SEXP y = PROTECT(allocVector(REALSXP,n));

  for(int i = 0; i < n; ++i)
    REAL(y)[i] = x[i];

  UNPROTECT(1);
  return y; 
}

SEXP asSEXP(MatrixXd x){
  int nr = x.rows();
  int nc = x.cols();
  SEXP y = PROTECT(allocMatrix(REALSXP,nr,nc));
  for(int i = 0; i < nr; ++i)
    for(int j = 0; j < nc; ++j)
      REAL(y)[i+nr*j] = x(i,j);

  UNPROTECT(1);
  return y;
}

MatrixXd asMatrix(SEXP x)
{

  if(!isMatrix(x))error("Element must be a matrix");

  int nr = nrows(x);
  int nc = ncols(x);
  MatrixXd y(nr,nc);

  for(int i = 0; i < nr; ++i)
    for(int j = 0; j < nc; ++j)
      y(i,j)=REAL(x)[i+nr*j];

  return y;
}

VectorXd asVector(SEXP x) {

  if(!isNumeric(x))error("Element must be a numeric vector");

  int n = length(x);
  VectorXd y(n);

  for(int i = 0; i < n; ++i)
    y(i) = REAL(x)[i];

  return y;
}


SEXP getListElement(SEXP list, const char *str)
{
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);

    for (R_len_t i = 0; i < length(list); i++)
        if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
           elmt = VECTOR_ELT(list, i);
           break;
        }
    return elmt;
}

VectorXd cumsum(VectorXd x){
  VectorXd y(x.size());
  y(0) = x(0);
  if(x.size()==1) 
    return y;

  for(int i = 1; i < x.size(); ++i){
    y(i) = y(i-1)+x(i);
  }
  return y;
}


MatrixXd rmvnorm(int n,VectorXd mu, MatrixXd sigma){
  //if(!(mu.size() == sigma.cols())) 
  //if(!(sigma.cols()==sigma.rows()))


  LLT<MatrixXd> llt(sigma);
  MatrixXd L = llt.matrixL();
  MatrixXd Y(sigma.cols(),n);
  GetRNGstate();
  for(int j = 0; j < Y.rows(); ++j)
    for(int i = 0; i < Y.cols(); ++i){
      Y(j,i) = norm_rand();
    }
  PutRNGstate();
  
  MatrixXd X = (L*Y).transpose();
  for(int i = 0; i < X.rows(); ++i){
    X.row(i) += mu;
  }
  return X;
}

extern "C" {
  SEXP rmvnorm(SEXP n,SEXP mu, SEXP sigma){
    int nn = asInteger(n);
    VectorXd mean = asVector(mu);
    MatrixXd sig = asMatrix(sigma);
    MatrixXd X = rmvnorm(nn,mean,sig);
    return asSEXP(X);
  }
}

// Sample from log normal

MatrixXd rmvlnorm(int n, VectorXd logmu, MatrixXd sigma){
  MatrixXd Y = rmvnorm(n,logmu,sigma);
  MatrixXd X = Y.array().exp().matrix();
  return X;
}

extern "C" {
  SEXP rmvlnorm(SEXP n,SEXP logmu, SEXP sigma){
    int nn = asInteger(n);
    VectorXd mean = asVector(logmu);
    MatrixXd sig = asMatrix(sigma);
    MatrixXd X = rmvlnorm(nn,mean,sig);
return asSEXP(X);
  }
}


// Sample from logistic normal

MatrixXd rlogistic(int n, VectorXd logimu, MatrixXd sigma){
  MatrixXd Y = rmvlnorm(n,logimu,sigma);
  MatrixXd X = Y;
  for(int i = 0; i < Y.rows(); ++i){
    X.row(i) *= 1/X.row(i).sum(); 
  }
  return X;
}

extern "C" {
  SEXP rlogistic(SEXP n,SEXP logimu, SEXP sigma){
    int nn = asInteger(n);
    VectorXd mean = asVector(logimu);
    MatrixXd sig = asMatrix(sigma);
    MatrixXd X = rlogistic(nn,mean,sig);
return asSEXP(X);
  }
}

// Sample from categorical distribution with categories 0:(length(probs)-1)

VectorXi rcategory(int n, VectorXd probs){
  int m = probs.size();
  VectorXd cumprobs = cumsum(probs);
  VectorXi Y(n);

  GetRNGstate();
  for(int i = 0; i < n; ++i){ // Samples
    double u = unif_rand();
    for(int j = 0; j < m; ++j){
      if(cumprobs(j) > u){
	Y(i) = j;
	break;
      }
    }
  }
  PutRNGstate();

  return Y;
}
extern "C" {
  SEXP rcategory(SEXP n,SEXP probs){
    int nn = asInteger(n);
    VectorXd prob = asVector(probs);
    VectorXi X = rcategory(nn,prob);
return asSEXP(X);
  }
}


// Sample from multivariate gaussian mixture

MatrixXd rmvgaussmix(int n, MatrixXd mu, Matrix<MatrixXd,Dynamic,1> sigma, VectorXd alpha){
  MatrixXd res(n,mu.rows());
  VectorXi I = rcategory(n,alpha);
  for(int i = 0; i < n; ++i){
    int j = I(i);
    res.row(i) = rmvnorm(1,mu.col(j),sigma(j));
  }
  return res;
}

extern "C" {
  SEXP rmvgaussmix(SEXP n, SEXP mu, SEXP sigma, SEXP alpha){
    Matrix<MatrixXd,Dynamic,1> sds(length(sigma));
    for(int i = 0; i < sds.rows(); ++i){
      sds(i) = asMatrix(VECTOR_ELT(sigma, i));
    }
    int nn = asInteger(n);
    MatrixXd mm = asMatrix(mu);
    VectorXd prob = asVector(alpha);
    MatrixXd res = rmvgaussmix(nn,mm,sds,prob);

    return asSEXP(res);
  }

}



// Sample from multivariate t-distribution
MatrixXd rmvt(int n,VectorXd mu, MatrixXd sigma, double df){
  //if(!(mu.size() == sigma.cols())) 
  //if(!(sigma.cols()==sigma.rows()))


  LLT<MatrixXd> llt(sigma);
  MatrixXd L = llt.matrixL();
  MatrixXd Y(sigma.cols(),n);
  GetRNGstate();
  for(int j = 0; j < Y.rows(); ++j){
    for(int i = 0; i < Y.cols(); ++i){
      Y(j,i) = norm_rand();
    }
  }
  PutRNGstate();
  
  MatrixXd X = (L*Y).transpose();
  for(int i = 0; i < X.rows(); ++i){
    double W = df / Rf_rchisq(df);
    X.row(i) *= sqrt(W);
    X.row(i) += mu;
  }
  return X;
}

extern "C" {
  SEXP rmvt(SEXP n,SEXP mu, SEXP sigma, SEXP df){
    int nn = asInteger(n);
    VectorXd loc = asVector(mu);
    MatrixXd sig = asMatrix(sigma);
    double dff = REAL(df)[0];
    MatrixXd X = rmvt(nn,loc,sig,dff);
    return asSEXP(X);
  }
}
