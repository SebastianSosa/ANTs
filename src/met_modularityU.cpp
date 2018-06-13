#include <Rcpp.h>
using namespace Rcpp;
NumericVector mat_cols_sums(NumericMatrix m);
// [[Rcpp::export]]
double met_modularityU(NumericMatrix M, NumericVector x) {
  NumericVector strength=mat_cols_sums(M);
  double m=sum(strength);
  
  double R=0;
  for(int a=0; a<M.ncol(); a++){
    for(int b=0; b<M.ncol(); b++){
      if(x[a]==x[b]){
        double Aij=M(a,b);
        Aij=(Aij-strength[a]*strength[b]/m)*1/m;
        R=R+Aij;
      }
    }
  }
  return(R);
}


