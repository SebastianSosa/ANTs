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
//' @title Vectors multiply
//' @description Multiply two vectors
//' @keywords internal
// [[Rcpp::export]]
NumericVector vec_vec_multiply(NumericVector x, NumericVector y)
{
  int nx = x.size(), ny = y.size();
  if(nx != ny) Rcpp::stop("vector x and vector y are not of the same length");
  int n = nx;
  NumericVector vxy(n);
  for (int i = 0; i < nx; i++)
    vxy[i] = x[i] * y[i];
  return vxy;
}
