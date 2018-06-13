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

NumericMatrix mat_symetrization(NumericMatrix M){
  int ncol=M.cols();
  NumericMatrix s(ncol,ncol);

  for(int a=0;a<ncol;a++){
    NumericVector row=M(a,_);
    NumericVector col=M(_,a);
    NumericVector all=row+col;
    
    s(a,_)=all;
    s(_,a)=all;
  }
  return s;
}