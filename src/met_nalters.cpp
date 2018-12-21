#include <Rcpp.h>
using namespace Rcpp;
NumericVector vec_id_sup0( NumericVector x);

// [[Rcpp::export]]
NumericVector met_nalters (NumericMatrix M){
  int S = M.ncol();
  NumericVector nalters(S);
  for(int a=0; a<S; a++){
    NumericVector r=M(a,_);
    NumericVector c=M(_,a);
    NumericVector degree=r+c;
    NumericVector alters=vec_id_sup0(degree);
    nalters[a]=alters.size();
  }
  return nalters;
}