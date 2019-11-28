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
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
double test_nm( Rcpp::NumericMatrix X ) {
  return 0.0 ;
}

// [[Rcpp::export]]
double test_arma( arma::mat X ) {
  return 0.0 ;
}

// [[Rcpp::export]]
double test_nm_conv( Rcpp::NumericMatrix X ) {
  arma::mat X_arma =  Rcpp::as<arma::mat>( X ) ; 
  return 0.0 ;
}
// [[Rcpp::export]]
double test_const_arma( const arma::mat& X ) { 
  return 0.0 ;
}

// [[Rcpp::export]]
arma::cx_mat met_ei2(const arma::mat& m) {
  arma::cx_vec eigval;
  arma::cx_mat eigvec;
  arma::eig_gen(eigval,eigvec, m);
  return eigvec;
}

