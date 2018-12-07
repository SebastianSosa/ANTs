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

#include <Rcpp.h>
using namespace Rcpp;
//' @title Control heterogeneity of time of observation
//'@description repeat a numeric vector n times, where n is the length of this vecctor
//' @author Sebastian Sosa, Ivan Puga-Gonzalez
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix tobs_to_mat(NumericVector vec){
  int n=vec.size();
  NumericMatrix obs(n,n);
  for(int a=0;a<n;a++){
    obs(a,_)=vec+vec(a);
    obs(a,a)=0;
  }
  //NumericMatrix result=M/obs;
  return obs;
}