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
//' @title Extract matrix row.
//' @description Extract a specific row of a matrix.
//' @param m a matrix.
//' @param int na integer indicating the row number to extract.
//' @return An integer vector corresponding the row extract.
//' @author Sebastian Sosa, Ivan Puga-Gonzales.
//' @keywords internal
// [[Rcpp::export]]
NumericVector mat_row_extract(NumericMatrix m, int x){
  NumericMatrix::Row r= m.row(x-1);
  NumericVector rvec = r;
  return rvec;
}
