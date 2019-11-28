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
using namespace arma;
NumericVector vec_match( CharacterVector x, CharacterVector y);
NumericVector mat_cols_sums(NumericMatrix m);
NumericVector mat_rows_sums(NumericMatrix x);

// [[Rcpp::export]]
List met_assor_cat(arma::mat& M,CharacterVector& att) {
  std::setprecision(7);
  CharacterVector uniques=unique(att);
  arma::mat Mixmat(uniques.size(),uniques.size());
  int S=Mixmat.n_cols;
  
  for(int a=0;a<S;a++){
    CharacterVector cat_id1(1);
    cat_id1(0)=uniques(a);
    NumericVector cat1=vec_match(att,cat_id1)- 1;
    
    for(int b=0; b<S;b++){
      CharacterVector cat_id2(1);
      cat_id2(0)=uniques(b);
      NumericVector cat2=vec_match(att,cat_id2)- 1 ;
      arma::uvec index1 = as<arma::uvec>(cat1);
      arma::uvec index2 = as<arma::uvec>(cat2);
      Mixmat(a,b)=arma::accu(M.submat(index1,index2));
    }
  }
  Mixmat=Mixmat/accu(Mixmat);
  arma::rowvec RSum= sum(Mixmat,0);
  colvec RSum1=conv_to< colvec >::from(RSum);
  arma::colvec CSum= sum(Mixmat,1);
  arma::vec R_C = RSum * CSum;
  double Value= sum (R_C);

  double r = (sum(Mixmat.diag()) - Value) / (1 - Value); 
  // r <- (sum(diag(m2)) - sum(rowSums(m2) * colSums(m2)))/(1 -sum(rowSums(m2) * colSums(m2)))
  List result (3);
  result(0)=r;
  result(1)=Mixmat;
  result(2)=uniques;
  return result;
}