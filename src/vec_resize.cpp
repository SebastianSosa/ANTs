
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
SEXP vec_resize(NumericVector vec, int x) {
  NumericVector res(vec.size()+x);
  return res;
}