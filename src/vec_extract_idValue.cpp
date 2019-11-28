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

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
double vec_num_extract(NumericVector x, int y) {
  NumericVector n=x.size();
  std::iota(n.begin(), n.end(),1);
  if(y>x.size()) stop("argument y is higher than the size of the vector x");
  if(y<1) stop("argument y is lower than the size of the vector x");
  double ids=0.000;
  for(int i=0; i<x.size();i++){
    if(n[i]==y+1){
      ids=x[i];
    }
  }
  return ids;
}
