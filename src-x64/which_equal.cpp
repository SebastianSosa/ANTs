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

#include <RcppArmadillo.h>
//' @title Equal
//' @description Find matrix element corresponding to an integer
//' @keywords internal
// [[Rcpp::export]]
arma::umat which_equal(arma::mat M,int x) {
   arma::uvec move = find(M==x);
   arma::umat idx  = ind2sub( size(M), move );
   return idx;
}