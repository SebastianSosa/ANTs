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

// !!!!!!!!!!! rand must be idexed as R !!!!!!!!!!!!!!!!!!!!!!!!!!
// [[Rcpp::export]]
NumericVector perm_matVec(NumericVector m, int n, IntegerVector rand){
  NumericVector result(n*n);
  int idx=0;
  
  for(int a=0;a<n;a++){
    int end=(rand(a)*n)-1;
    
    int start=end-(n-1);
    
    for(int b=0;b<n;b++){
      result(idx)=m(start+(rand(b)-1));
      idx=idx+1;
    }
    
  }
  return result;
} 
