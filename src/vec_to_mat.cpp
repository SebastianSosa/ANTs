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

//' @title Vector to matrix
//' @description Create a matrix from a vector
//' @param vec a numeric vector
//' @param n number of columns of the matrix to create
//' @param diag a boolean indicating with the argument vec have the diagonal information in it.
//' @return A square matrix
//' @author Sebastian Sosa.
//' @keywords internal

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix vec_to_mat(NumericVector vec, int ncol, bool diag) {
  NumericMatrix M(ncol,ncol);
  if(diag){
    int mem=0;
    for(int a=0;a<ncol;a++){
      int start=a+mem;
      int end=start+ncol;
      NumericVector tmp(ncol);
      std::copy(vec.begin()+start, vec.begin()+end, tmp.begin());
      M(_,a)=tmp;
      mem=mem+(ncol-1);
    }
  }
  else{
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
  }
  return M;
}

/*** R
ANTs:::vec_to_mat(c(1:25),5,T)
*/