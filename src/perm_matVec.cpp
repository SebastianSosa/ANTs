// Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
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


//' @title Vectorize matrix permutation
//' @description Permute rows and columns of a vectorized matrix
//' @param vec a vumeric matrix
//' @param n number of columns of the original matrix
//' @param rand a numeric vector of randomized column and rows indeces
//' @details It's the equivalent of R m[rand, rand] function. Columns and rows indexatons are in R format.
//' @return A vector
//' @author Sebastian Sosa.
//' @keywords internal

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector perm_matVec(NumericVector vec, int n, IntegerVector rand){
  NumericVector result(n*n);
  int idx=0;
  
  for(int a=0;a<n;a++){
    int end=(rand(a)*n)-1;
    
    int start=end-(n-1);
    
    for(int b=0;b<n;b++){
      result(idx)=vec(start+(rand(b)-1));
      idx=idx+1;
    }
    
  }
  return result;
} 

/*** R
matrix(ANTs:::perm_matVec(vec = 1:50, n = 5, rand = c(5,4,2,1,3)),5,5)

matrix(1:50,5,5)[c(5,4,2,1,3), c(5,4,2,1,3)]
*/
