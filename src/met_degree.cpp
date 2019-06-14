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
NumericVector vec_id_sup0(NumericVector x);

// [[Rcpp::export]]
NumericVector met_degree(NumericMatrix M) {
  int S = M.ncol();
  NumericVector degree(S);
  for(int a=0; a<S; a++){
    NumericVector r=M(a,_);
    NumericVector c=M(_,a);
    NumericVector row=vec_id_sup0(r);
    NumericVector col=vec_id_sup0(c);
    degree[a]=row.size()+col.size();
  }
  return degree;
}
