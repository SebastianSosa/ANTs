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
//' @title Vector to matrix and adding diag
//' @description Create a matrix from a vector
//' @param vec a numeric vector
//' @param n number of columns of the matrix to create
//' @return A square matrix
//' @author Sebastian Sosa.
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix vec_to_mat_add_diag (NumericVector vec, int ncol) {
  NumericMatrix M(ncol,ncol);
  int mem=0;
  for(int a=0;a<ncol;a++){
    for(int b=0;b<ncol;b++){
      if(a==b){
        M(b,a)=NA_REAL;
      }
      else{
        M(b,a)=vec(mem);
        mem=mem+1;
      }
    }
  }
  return M;
}
