// Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He, Xiaohua Xie, Cédric Sueur
//
// This file is part of Animal Network Toolkit Software (ANTs).
//
// ANT is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// ANT is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

#include <Rcpp.h>
using namespace Rcpp;
//' @title Vector permutations.
//' @description permutes elements of a vector. 
//' @param x a numeric vector.
//' @param perm an integer indicating the number of permutations to perform.
//' @param progress a boolean indicating if you wich to see the progression of the permutations.
//' @return A list of vectors, each ones holding a different permutation.
//' @details  Permuting a vector can serve to realize node label permutation.
//' @author Sebastian Sosa,Ivan Puga-Gonzales.
//' @references Farine, D. R. (2017). A guide to null models for animal social network analysis. Methods in Ecology and Evolution.
//' @references Sosa, S. (\emph{in press}). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List perm_vec_int(NumericVector x, int nperm, bool progress) {
  Rcpp::List permuted(nperm);
  for(int a=0; a<nperm;a++){
  	    if(progress==TRUE){
      Rcpp::Rcout<<"\r"<<"permutation: "<<a;
      Rcpp::Rcout.flush();
    }

    Rcpp::Rcout<<"\r"<<"permutation: "<<a;
    Rcpp::Rcout.flush();
    permuted[a]=Rcpp::sample(x,x.size(), false);
  }
  return permuted;
}

