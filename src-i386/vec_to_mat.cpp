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
NumericMatrix vec_to_mat(NumericVector vec, int ncol) {
  NumericMatrix M(ncol,ncol);
  int mem=0;
  for(int a=0;a<ncol;a++){
    int start=a+mem;
    int end=start+ncol;
    NumericVector tmp(ncol);
    std::copy(vec.begin()+start, vec.begin()+end, tmp.begin());
    M(_,a)=tmp;
    mem=mem+(ncol-1);
  }
  return M;
}