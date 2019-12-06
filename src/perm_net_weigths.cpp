#include <Rcpp.h>
using namespace Rcpp;
NumericVector vec_id_sup0( NumericVector x);
NumericMatrix edgl_to_matrix(DataFrame df, bool sym);
SEXP vec_sample_all(SEXP vec);


//' @title Edgelist weigths permutations
//' @description Post-network permutations on links weights.
//' @keywords internal
// [[Rcpp::export]]
List perm_net_weigths(DataFrame df, bool sym, int nperm, bool progress ) {
  
  List result(nperm+1);
  result[0] = edgl_to_matrix(df, sym);
  DataFrame d = clone(df);
  
  if(progress){
    for(int a=1; a < nperm+1; a++){
      // Print permutations progress
      Rcpp::Rcout<<"\r"<<"Permutation: "<<a;
      Rcpp::Rcout.flush();

      // Extract link weigths
      NumericVector w = d["weight"];
      // Permute them
      SEXP tmp = vec_sample_all(w);
      d["weight"] = tmp;
      result[a] = edgl_to_matrix(d, sym);
    }
  }
  else{
    for(int a=1; a < nperm+1; a++){
      // Extract link weigths
      NumericVector w = d["weight"];
      // Permute them
      SEXP tmp = vec_sample_all(w);
      d["weight"] = tmp;
      result[a] = edgl_to_matrix(d, sym);
    }
  }
  return result;
}
