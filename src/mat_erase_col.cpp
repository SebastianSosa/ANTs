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
// [[Rcpp::export]]
NumericMatrix mat_erase_col(NumericMatrix M,int a){
  NumericMatrix x2(M.nrow(), (M.ncol()-1) );
  int mem=0;
  for (int i = 0; i < M.ncol(); i++) {
    bool test=i != a;
    std::cout<<test<<std::endl;
    if (i != a) {
      x2(_,mem) = M(_,i);
      mem++;
    }
  }
  return x2;
}