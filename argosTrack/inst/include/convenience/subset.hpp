
#ifndef _SUBSET_VECTOR_MATRIX_
#define _SUBSET_VECTOR_MATRIX_


template <class Type>
vector<Type> subset(vector<Type> x, vector<Type> keep){
  int nl = CppAD::Integer(keep.sum());
  vector<Type> res(nl);

  int inres = 0;
  for(int i = 0; i < x.size(); ++i){
    if(CppAD::Integer(keep(i)) == 1){
      res(inres) = x(i);
      ++inres;
    }
  }
  return res;
}

// Assumed to be a covariance matrix
template <class Type>
matrix<Type> subset(matrix<Type> x, vector<Type> keep){
  int nl = CppAD::Integer(keep.sum());
  matrix<Type> res(nl,nl);

  int inres = 0;
  for(int i = 0; i < x.cols(); ++i){
    if(CppAD::Integer(keep(i)) == 1){
      res.col(inres) = subset((vector<Type>)x.col(i),keep);
      ++inres;
    }
  }
  return res;
}

#endif
