#define WITH_LIBTMB
#include <TMB.hpp>

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


extern "C" {

  // Sample
  // SEXP rmvnorm(SEXP n,SEXP mu, SEXP sigma);
  // SEXP rmvlnorm(SEXP n,SEXP logmu, SEXP sigma);
  // SEXP rlogistic(SEXP n,SEXP logimu, SEXP sigma);
  // SEXP rcategory(SEXP n,SEXP probs);
  // SEXP rmvgaussmix(SEXP n, SEXP mu, SEXP sigma, SEXP alpha);
  // SEXP rmvt(SEXP n,SEXP mu, SEXP sigma, SEXP df);
  // SEXP rgig(SEXP n,SEXP lambda, SEXP a, SEXP b);

  // Convenience
  SEXP ll2n(SEXP lon, SEXP lat);
  SEXP n2ll(SEXP x, SEXP y);
  SEXP idtcrwVarMat(SEXP dt , SEXP gamma, SEXP phi, SEXP rho, SEXP varState);
  SEXP stepLength(SEXP x0, SEXP y0, SEXP x1, SEXP y1, SEXP nautical);
  SEXP bearing(SEXP x0, SEXP y0, SEXP x1, SEXP y1, SEXP nautical);
  SEXP kroneckersum(SEXP x, SEXP y);

#define CALLDEF(name,n) {#name, (DL_FUNC) &name, n}
  
  static const
  R_CallMethodDef callMethods[] = {

    // TMB
    #ifdef TMB_CALLDEFS
    TMB_CALLDEFS,
    #else
    CALLDEF(MakeADFunObject, 4),
    CALLDEF(InfoADFunObject, 1),
    CALLDEF(EvalADFunObject, 3),
    CALLDEF(MakeDoubleFunObject, 3),
    CALLDEF(EvalDoubleFunObject, 3),
    CALLDEF(getParameterOrder, 3),
    CALLDEF(MakeADGradObject, 3),
    CALLDEF(MakeADHessObject2, 4),
    CALLDEF(usingAtomics, 0),
    CALLDEF(TMBconfig, 2),
    #endif
    
    // CALLDEF(rmvnorm,3),
    // CALLDEF(rmvlnorm,3),
    // CALLDEF(rlogistic,3),
    // CALLDEF(rcategory,2),
    // CALLDEF(rmvgaussmix,4),
    // CALLDEF(rmvt,4),
    // CALLDEF(rgig,4),
    CALLDEF(ll2n,2),
    CALLDEF(n2ll,2),
    CALLDEF(idtcrwVarMat,5),
    CALLDEF(stepLength,5),
    CALLDEF(bearing,5),
    CALLDEF(kroneckersum,2),
    {NULL,NULL,0}
  };

  void R_init_argosTrack(DllInfo *info)
  {
    /* Register the .C and .Call routines.
       No .Fortran() or .External() routines,
       so pass those arrays as NULL.
    */
    R_registerRoutines(info,
		       NULL, callMethods,
		       NULL, NULL);
    R_useDynamicSymbols(info, (Rboolean)FALSE);
  }


}
