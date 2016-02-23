
#ifndef _BESSELI_
#define _BESSELI_


  extern "C" {
    /* See comment to namespace atomic::Rmath https://github.com/kaskr/adcomp/blob/master/TMB/inst/include/atomic_math.hpp */
    double	Rf_bessel_i(double, double, double);
  }


/** \brief Atomic version of \f$besselI(x,\nu)\f$.
    Valid parameter range: \f$x =(x,\nu) \in \mathbb{R}_+\times\mathbb{R}\f$.
    \note Derivative wrt. \f$\nu\f$ is currently not implemented.
    \param x Input vector of length 2.
    \return Vector of length 1.
*/

TMB_ATOMIC_VECTOR_FUNCTION(
			   // ATOMIC_NAME
			   besselI
			   ,
			   // OUTPUT_DIM
			   1
			   ,
			   // ATOMIC_DOUBLE
			   ty[0] = Rf_bessel_i(tx[0], tx[1], 1.0 /* Not scaled */);
			   ,
			   // ATOMIC_REVERSE
			   Type value = ty[0];
			   Type x = tx[0];
			   Type nu = tx[1];
			   CppAD::vector<Type> arg(2);
			   CppAD::vector<Type> arg2(2);
			   arg[0] = x;
			   arg[1] = nu + Type(1);
			   arg2[0] = x;
			   arg2[1] = nu - Type(1);
			   px[0] = ( Type(0.5) * ( besselI(arg)[0] + besselI(arg2)[0] ) ) * py[0];
			   px[1] = Type(0); /* Not yet implemented (!) */
			   )


template<class Type>
Type besselI(Type x, Type nu){
  if (CppAD::Variable(nu)) error("besselI(x,nu) does not yet allow 'nu' to be a PARAMETER.");
  CppAD::vector<Type> tx(2);
  tx[0] = x;
  tx[1] = nu;
  return besselI(tx)[0];
}


#endif
