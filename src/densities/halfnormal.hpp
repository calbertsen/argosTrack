
#ifndef _HALFNORMAL_
#define _HALFNORMAL_

// density of the half normal distribution Y = |X| where X~N(0,\sigma^2)

template<class Type>
Type dhalfnorm(Type x, Type sd, int give_log = 0){
  Type var = pow(sd,Type(2.0));
  Type logres2 = log(Type(2.0)) - pow(x,Type(2.0)) / var - log(var*M_PI);
  if(give_log) return Type(0.5)*logres2; else return exp(Type(0.5)*logres2);
}

VECTORIZE3_tti(dhalfnorm);



#endif
