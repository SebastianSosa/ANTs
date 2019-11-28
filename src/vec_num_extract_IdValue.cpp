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
//' @title Extract ID
//' @description Extract ID of element selected by a numeric vector
//' @keywords internal
// [[Rcpp::export]]

Rcpp::NumericVector vec_num_extract_IdValue(NumericVector y, NumericVector x){
  NumericVector elements=x.size();
  for(int a=0;a<x.size();a++){
    int select=x(a);
    select=select-1;
    elements[a]=y(select);
  }
  return elements;
}
