

#ifndef _MULTIVARIATE_T_AND_NORMAL_
#define _MULTIVARIATE_T_AND_NORMAL_

using density::MVNORM_t;

/** \brief Multivariate t distribution with user supplied scale matrix

Class to evaluate the negative log density of a multivariate t distributed variable with general scale matrix Sigma and location vector 0 and df degrees of freedom.
*/
template <class Type>
class MVT_tt: public MVNORM_t<Type>
{
  Type df;
  /* modelCode: 
     0: Student's t-distribution
     1: Normal distribution
     2: Symmetric Hyperbolic distribution
   */
  int modelCode;
  Type shnc;

public:
  MVT_tt()
    : MVNORM_t<Type>()
  {  };
  // MVT_tt(Type df_)
  //   : MVNORM_t<Type>()
  // {
  //   df = df_;
  //   modelCode = 0;
  // }
  // MVT_tt(matrix<Type> Sigma_, Type df_)
  //   : MVNORM_t<Type>(Sigma_)
  // {
  //   df = df_;
  //   modelCode = 0;
  // }
  MVT_tt(matrix<Type> Sigma_, Type df_, int modelCode_)
    : MVNORM_t<Type>(Sigma_), shnc(0.0)
  {
    df = df_;
    modelCode = modelCode_;
    if(modelCode == 2){
      if(Sigma_.cols() == 2){
	shnc = symHyperbolicConst(Type(0.0),Type(0.0),df_,
				  sqrt(Sigma_(0,0)),
				  sqrt(Sigma_(1,1)),
				  Sigma_(1,0) / sqrt(Sigma_(0,0) * Sigma_(1,1)));
      }else{
	error("Symmetric Hyperbolic can only be used in 2D.");
      }
    }
  }

  void setdf(Type df_){
    df = df_;
  }

  /** \brief Evaluate the negative log density */
  Type operator()(vector<Type> x){
    Type p = x.size();
    Type tdens = -lgamma(Type(0.5)*(df+p))+lgamma(Type(0.5)*df)+p*Type(0.5)*log(df)+p*lgamma(Type(0.5))-Type(0.5)*this->logdetQ + Type(0.5)*(df+p)*log(Type(1.0)+this->Quadform(x)/df);
    Type ndens = -Type(.5)*this->logdetQ + Type(.5)*this->Quadform(x) + p*Type(log(sqrt(2.0*M_PI)));
    Type hdens = 0.0;
    if(modelCode == 2){
      hdens = shnc + sqrt(df*df + this->Quadform(x));
    }
    
    switch(modelCode){
    case 0: return tdens;
    case 1: return ndens;
    case 2: return hdens;
    default: return Type(1.0/0.0);
    }
  }

   /** \brief Evaluate _projected_ negative log density
      \param keep Vector of 0/1 indicating marginal to evaluate.
   */
  // Type operator()(vector<Type> x, vector<Type> keep){
  //   matrix<Type> S = subset(this->Sigma,keep);
  //   MVNORM_t<Type> newmvn(S);
  //   vector<Type> nx = subset(x,keep);

  //   Type p = nx.size();
  //   //Lange et al. 1989 http://www.jstor.org/stable/2290063
  //   Type tdens = -lgamma(Type(0.5)*(df+p))+lgamma(Type(0.5)*df)+p*Type(0.5)*log(df)+p*lgamma(Type(0.5))-Type(0.5)*newmvn.logdetQ + Type(0.5)*(df+p)*log(Type(1.0)+newmvn.Quadform(nx)/df);
  //   Type ndens = -Type(.5)*newmvn.logdetQ + Type(.5)*newmvn.Quadform(nx) + p*Type(log(sqrt(2.0*M_PI)));
  //   Type hdens = mySH(x);

  //   switch(modelCode){
  //   case 0: return tdens;
  //   case 1: return ndens;
  //   case 2: return hdens;
  //   default: return Type(1.0/0.0);
  //   }
  // }

  
};




#endif
