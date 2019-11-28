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
//' @title Vector to matrix lower triangle
//' @description Create a matrix from a vector 
//' @param vec a numeric vector
//' @param col number of columns of the matrix to create
//' @param diag doe diagonal is included
//' @section Warning: Argument vec must be of the exact same length of the number of cells present in the lower triangle of the amtrix to create. 
//' @return A vector
//' @author Sebastian Sosa.
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix vec_lowertri_to_mat(NumericVector& vec, int col, bool diag) {
  
  if(diag==false){
    NumericMatrix M(col,col);
    NumericMatrix& Mpoint = M;
    
    int mem = 0;
    for(int a=0; a<col-1; a++){
      int starts = mem;
      int end = mem + (col - a-1);
      NumericVector tmp_vec(col);
      NumericVector& vec2 = tmp_vec;
      std::copy(vec.begin()+starts,vec.begin()+end,vec2.begin()+a+1);
      Mpoint(_,a)=vec2;
      mem=end;
    }
    return M;
  }
  
  else{// Not working !!!!!!!!!!!!!!!!!!!!!!!!!!!
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

