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
//' @title strength
//' @description Calculate for all the vertices the node metric call strentgh.
//' @param M a square adjacency matrix.
//' @return Integer vector of each vertices outstrentgh.
//' @details  strength of vertice \emph{i} is all edges //' @title Outstrength
//' @description Calculate for all the vertices the node metric call outstrentgh.
//' @param M a square adjacency matrix.
//' @return Integer vector of each vertices outstrentgh.
//' @author Sebastian Sosa, Ivan Puga-Gonzales.
//' @references Barrat, A., Barthelemy, M., Pastor-Satorras, R., & Vespignani, A. (2004). The architecture of complex weighted networks. Proceedings of the National Academy of Sciences of the United States of America, 101(11), 3747-3752.
//' @references Sosa, S. (\emph{in press}). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
//' @keywords internal
// [[Rcpp::export]]

arma::rowvec met_strength(arma::mat M){
  arma::rowvec colsum = sum(M);
  arma::colvec rowsum = sum(M,1);
  arma::rowvec rowsum2= arma::conv_to< arma::rowvec >::from(rowsum);
  arma::rowvec result= colsum+rowsum2;
  return result;
}
