// Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
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
//' @title Eigenvector centrality
//' @description compute the eigenvecto centrality for each vertices through Armadillo c++ library.
//' @param m a matrix.
//' @return A numeric vector corresponding to the Eigenvector centrality of each nodes.
//' @details EigenvectorCpp centrality is the first non-negative met.eigenvector value obtained through the linear transformation of an adjacency matrix. This centrality measure quantifies not only a node connectedness, but also the connections of the nodes to whom it is connected. Thus, a node can have a high met.evcent value by having a high met.degree or met.strength, or by being connected to nodes that have high degrees or strengths.
//' @author Sebastian Sosa, Ivan Puga-Gonzales.
//' @references Sosa, S. (\emph{in press}). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
//' @keywords internal
// [[Rcpp::export]]
arma::cx_mat met_ei(arma::mat m) {
  arma::cx_vec eigval;
  arma::cx_mat eigvec;
  arma::eig_gen(eigval,eigvec, m);
  return eigvec;
}

