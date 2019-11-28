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
//' @title Dimensions of a matrix
//' @description Retrieve or set the dimension of Matrix.
//' @param m a matrix.
//' @return A numeric vector with two elements. The first one correspond to the number of row and the second one correspond to the number of columns.
//' @author Sebastian Sosa, Ivan Puga-Gonzales.
//' @keywords internal
// [[Rcpp::export]]

NumericVector mat_dim(NumericMatrix m) {
  int x = m.nrow(), y = m.ncol();
  NumericVector result(2);
  result(0)=x;
  result(1)=y;
  return result;
}
