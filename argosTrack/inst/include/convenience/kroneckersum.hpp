
#ifndef _KRONECKERSUM_
#define _KRONECKERSUM_

template<class Type>
matrix<Type> kroneckersum(matrix<Type> x, matrix<Type> y){
  matrix<Type> Ix(x.rows(),x.rows());
  Ix.setIdentity();
  matrix<Type> Iy(y.rows(),y.rows());
  Iy.setIdentity();    

  return kronecker(x,Iy) + kronecker(Ix,y);
}



//Export to R
extern "C" {

  
  SEXP kroneckersum(SEXP x, SEXP y){
    return(asSEXP(kroneckersum(asMatrix<double>(x),asMatrix<double>(y))));   
  }

}


#endif
