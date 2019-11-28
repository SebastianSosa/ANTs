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
#include <iomanip>
using namespace Rcpp;
NumericVector mat_row_wise_multiplication(NumericMatrix m, NumericVector v);
double euclidean(NumericVector vec);
NumericVector vector_abs(NumericVector vec) ;

// [[Rcpp::export]]
NumericVector met_eigen(NumericMatrix M, double eps=0.000001, int maxiter=1000) {
  int M_size= M.ncol();
  NumericVector v(M_size, 1.0);
    
  std::setprecision(8);
  
  NumericVector v2=v;
  NumericVector eigen(M_size);
  int count=1;
  
  
  while(count < maxiter){
    eigen=mat_row_wise_multiplication(M,v2);

    double tmp=euclidean(eigen);

    eigen=eigen/tmp;

    NumericVector abs_v2=vector_abs(v2) ;
    NumericVector abs_eigen=vector_abs(eigen) ;

    if((sqrt(sum(abs_eigen-abs_v2))<= eps)==true){break;}
    
    v2=eigen;
    count = count + 1;
    if(maxiter==count){break;}
  }
  
  NumericVector result=eigen/max(eigen);
  
  return result;
    
}
