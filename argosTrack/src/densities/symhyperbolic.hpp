

#ifndef _SYMMETRIC_HYPERBOLIC_ATOMIC_
#define _SYMMETRIC_HYPERBOLIC_ATOMIC_

namespace argosTrack_atomic {


  template<class Float>
  struct symHyperbolic2D_t {
    typedef Float Scalar;
    Float x, y;  // Integration variables
    Float theta, sdx, sdy, rho;  // Parameters

    Float operator() () {
      Float r2 = rho * rho;
      Float r2m1 = r2 - Float(1.0);
      Float sdx2 = sdx * sdx;
      Float sdy2 = sdy * sdy;
      //Float detQ = 1.0 / (-r2 * sdx2 * sdy2 + sdx2 * sdy2);
      Float Quadform = 2.0 * rho * sdx * sdy * x * y - sdx2 * y * y - sdy2 * x * x;
      Quadform /= sdx2 * sdy2 * r2m1;
      Float ans = exp( - sqrt(theta*theta + Quadform)); // * sqrt(detQ);
      return ans;
    }
    
    // Integrate wrt (x,y)
    Float nconst() {
      using gauss_kronrod::mvIntegrate;
      Float ans = mvIntegrate(*this).
	wrt(x, -INFINITY, INFINITY).
	wrt(y, -INFINITY, INFINITY) ();
      return ans;
    }
  };

  template<class Float>
  Float evalSymHyperbolic2D(Float x, Float y,Float theta, Float sdx,Float sdy, Float rho) {
    symHyperbolic2D_t<Float> f = {x,y,theta,sdx,sdy,rho};
    Float v1 = f();
    Float v2 = f.nconst();
    return -log(v1 + 1e-12) + log(v2 + 1e-12);
  }

  TMB_BIND_ATOMIC(atomicSymHyperbolic, 111111, evalSymHyperbolic2D(x[0], x[1], x[2], x[3], x[4], x[5]))

  template<class Float>
  Float evalSymHyperbolic2DConst(Float x, Float y,Float theta, Float sdx,Float sdy, Float rho) {
    symHyperbolic2D_t<Float> f = {x,y,theta,sdx,sdy,rho};
    Float v2 = f.nconst();
    return log(v2 + 1e-12);
  }

  TMB_BIND_ATOMIC(atomicSymHyperbolicConst, 001111, evalSymHyperbolic2DConst(x[0], x[1], x[2], x[3], x[4], x[5]))

}
  
  template<class Type>
  Type symHyperbolic(Type x, Type y, Type theta, Type sdx, Type sdy, Type rho) {
    vector<Type> args(7); // Last index reserved for derivative order
    args[0] = x;
    args[1] = y;
    args[2] = theta;
    args[3] = sdx;
    args[4] = sdy;
    args[5] = rho;
    args[6] = 0;
    return argosTrack_atomic::atomicSymHyperbolic(CppAD::vector<Type>(args))[0];
  }

  template<class Type>
  Type symHyperbolicConst(Type x, Type y, Type theta, Type sdx, Type sdy, Type rho) {
    vector<Type> args(7); // Last index reserved for derivative order
    args[0] = x;
    args[1] = y;
    args[2] = theta;
    args[3] = sdx;
    args[4] = sdy;
    args[5] = rho;
    args[6] = 0;
    return argosTrack_atomic::atomicSymHyperbolicConst(CppAD::vector<Type>(args))[0];
  }



#endif
