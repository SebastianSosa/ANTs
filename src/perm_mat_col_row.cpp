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

// [[Rcpp::export]]
NumericMatrix perm_mat_col_row(NumericMatrix M, int ncol, IntegerVector rand) {
  NumericMatrix M2(ncol,ncol);
  for(int a = 0; a < rand.size(); a++){
    for(int b = 0 ; b < rand.size(); b++){
      M2(a,b)= M(rand(a), rand(b));
    }
  }
  return M2;
}
