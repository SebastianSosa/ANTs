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
//' @title Binary version of column sum.
//' @description Count the number of non empty cells of each columns of an adjacency matrix.
//' @param m a matrix.
//' @return An integer vector. Each elements correspond to the sum of non empty cells according to the corresponding column.
//' @author Sebastian Sosa, Ivan Puga-Gonzales.
//' @keywords internal
// [[Rcpp::export]]
NumericVector mat_col_sumsBinary(NumericMatrix m) {
  int nrow = m.nrow(), ncol = m.ncol();
  NumericVector out(ncol);
  for (int i = 0; i < ncol; i++) {
    double total = 0;
    for (int j = 0; j < nrow; j++) {
      if(m(j, i)>0) total += +1;
    }
    out[i] = total;
  }
  return out;
}
