
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
Type bearing (Type x0, Type y0, Type x1, Type y1, bool nautical = true){

    vector<Type> dx(2);
  
  if(nautical){
    dx(0) = x1-x0;
    dx(1) = y1-y0;
  }else{
    dx = ll2n(x1,y1) - ll2n(x0,y0);
  }

  return atan2(dx(1),dx(0));
}

#endif
