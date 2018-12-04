// Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
//
// This file is part of Animal Network Toolkit (ANT).
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

// [[Rcpp::export]]
NumericMatrix vec_lowertri_to_mat(NumericVector& vec, int col, bool diag) {
  if(diag==false){
    NumericMatrix M(col,col);
    NumericMatrix& Mpoint=M;
    int mem=0;
    for(int a=0;a<col-1;a++){
      int starts= mem;
      int end= mem+(col-a-1);
      NumericVector tmp_vec(col);
      NumericVector& vec2=tmp_vec;
      std::copy(vec.begin()+starts,vec.begin()+end,vec2.begin()+a+1);
      Mpoint(_,a)=vec2;
      mem=end;
    }
    return M;
  }
  
  else{
    NumericMatrix M(col,col);
    NumericMatrix& Mpoint=M;
    int mem=0;
    for(int a=0;a<col-1;a++){
      int end= mem+(col-a);
      NumericVector tmp_vec(col);
      NumericVector& vec2=tmp_vec;
      Mpoint(_,a)=vec2;
      mem=end;
    }
    return M;
  }

}

