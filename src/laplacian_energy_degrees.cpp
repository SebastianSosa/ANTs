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

double laplacian_energy_degrees_C(NumericMatrix M) {
  int S=M.ncol();
  NumericVector degrees(S);
  for(int a=0; a<S; a++){
    NumericVector col=M(_,a);
    NumericVector row=M(a,_);
    
    double degree=sum(col+row);
    degrees[a]=degree;
    
    NumericVector square_col=col*col;
    NumericVector square_row=row*row;
    M(_,a)=square_col;
    M(a,_)=square_row;
  }
  double d1=sum(degrees*degrees);
  double w1=sum(M);
  return d1+w1;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
laplacian_energy_degrees<-function(M){
  degrees=met.degree(M)
  d1=sum(degrees^2)
  w1=sum(M^2)
  
  le=d1+w1
  return(le)
}

*/
