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
//' @title Is square.
//' @description Test if a matrix is square or not.
//' @param m a matrix.
//' @return a boolean indicating if the matrix is square or not.
//' @author Sebastian Sosa, Ivan Puga-Gonzales.
//' @keywords internal
// [[Rcpp::export]]

bool mat_isSquare(NumericMatrix m)
{
  int nrow = m.nrow(), ncol = m.ncol();
  if(nrow==ncol){
    return true;
  }else{
    return false;
  }
}
