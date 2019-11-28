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

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
//' @title Matrix filtering
//' @description Filtering matrix cells according to a threshold and replace them by a bouble.
//' @param m a matrix.
//' @param threshold a value above wich the cells of the matrix will be keep.
//' @param replace a doubl eindicating the value by which replace the replace elements.
//' @return A matix.
//' @author Sebastian Sosa, Ivan Puga-Gonzales.
//' @keywords internal
// [[Rcpp::export]]
arma::mat mat_filter(arma::mat m,double threshold, double replace) {
  arma::uvec ids = find(m >=threshold); // Find indices
  m.elem(ids).fill(replace);       // Assign value to condition
  return m;
}
