
#ifndef _STEPLENGTHBEARING_
#define _STEPLENGTHBEARING_

template<class Type>
Type stepLength(Type x0, Type y0, Type x1, Type y1, bool nautical = true){

  vector<Type> dx(2);
  
  if(nautical){
    dx(0) = x1-x0;
    dx(1) = y1-y0;
  }else{
    dx = ll2n(x1,y1) - ll2n(x0,y0);
  }

  return sqrt(sum(pow(dx,Type(2.0))));
}


template<class Type>
Type atan3(Type y, Type x){

{	Type alpha;
	Type beta;
	Type theta;

        Type zero(0.);
        Type pi2(2. * atan(1.));
	Type pi(2. * pi2);

	Type ax = abs(x);
	Type ay = abs(y);
	
	// if( ax > ay )
	// 	theta = atan(ay / ax);
	// else	theta = pi2 - atan(ax / ay);
	alpha = atan(ay / (ax + 1e-6));
	beta  = pi2 - atan(ax / (ay + 1e-6));
	theta = CppAD::CondExpGt(ax, ay, alpha, beta);         // use of CondExp

	// if( x <= 0 )
	// 	theta = pi - theta;
	theta = CppAD::CondExpLe(x, zero, pi - theta, theta);  // use of CondExp
	
	// if( y <= 0 )
	// 	theta = - theta;
	theta = CppAD::CondExpLe(y, zero, -theta, theta);      // use of CondExp

	// if( x == y == 0) <=> if( ax+ay == 0)
	// theta = 0
	theta = CppAD::CondExpEq(ax+ay, zero, zero, theta);
	
	return theta;
}

}

template<class Type>
Type bearing(Type x0, Type y0, Type x1, Type y1, bool nautical = true){

    vector<Type> dx(2);
  
  if(nautical){
    dx(0) = x1-x0;
    dx(1) = y1-y0;
  }else{
    dx = ll2n(x1,y1) - ll2n(x0,y0);
  }

  return atan3(dx(1),dx(0));
}


extern "C" {

  SEXP stepLength(SEXP x0, SEXP y0, SEXP x1, SEXP y1, SEXP nautical){
    return asSEXP(stepLength(asDouble(x0),asDouble(y0),asDouble(x1),asDouble(y1),asBool(nautical)));
  }

  SEXP bearing(SEXP x0, SEXP y0, SEXP x1, SEXP y1, SEXP nautical){
    return asSEXP(bearing(asDouble(x0),asDouble(y0),asDouble(x1),asDouble(y1),asBool(nautical)));
  }
}


#endif
