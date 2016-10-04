

#ifndef _MULTIVARIATE_SYMMETRIC_HYPERBOLIC_
#define _MULTIVARIATE_SYMMETRIC_HYPERBOLIC_




template<class Type>
struct dmvsh {
  Type theta;               // Shape parameter
  matrix<Type> Q;               // Scale parameter
  Type detQ;

  dmvsh(){};
  
  dmvsh(Type theta_, matrix<Type> Sigma) 
    : theta (theta_) {
    Q = Sigma.inverse();
    detQ = Q.determinant();
  }

  Type Quadform(vector<Type> x){
    return (x*(vector<Type>(Q*x))).sum();
  }
  
  Type operator()(vector<Type> x){
    return exp( - sqrt(theta + theta * Quadform(x) ))*sqrt(detQ);
  }
};


template<class Type>
class MVSH_t {
  matrix<Type> Sigma;
  Type theta;
  vector<Type> a;
  vector<Type> b;
  dmvsh<Type> f;
  Type normConst;

public:
  MVSH_t(){};

    // : f(0.0,matrix<Type>(1,1).Identity()), theta(0.0), Sigma(matrix<Type>(1).Identity()), a(vector<Type>(1)+Type(-10.0)), b(vector<Type>(1)+Type(10.0))
  // {
  //   normConst = 0; //romberg::integrate(f, a, b, 10);
  //   Rcout << normConst << "\n";
  // };
  
  MVSH_t(Type theta_, matrix<Type> Sigma_, vector<Type> a_, vector<Type> b_, int n) : f(theta_,Sigma_), theta(theta_), Sigma(Sigma_), a(a_), b(b_) {
    normConst = romberg::integrate(f, a, b, n);
  }

  Type operator()(vector<Type> x){
    return -log(f(x)) + log(normConst);
  }
};


#endif
