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
#include <iostream>
#include <vector>
#include <algorithm>

using namespace Rcpp;
//' @title Unmatch c
//' @description Find element that doesn't corespond with the secon vector with c indexation
//' @keywords internal
// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
NumericVector vec_unmatch_indexcc( CharacterVector x, CharacterVector y){
  IntegerVector test= match(x,y);
  NumericVector ids;
  for(int i=0; i<x.size();i++){
    if(test[i]==NA_INTEGER){
      ids.push_back(i);
    }
  }
  return ids;
}
//