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
NumericVector mat_col_sumsBinary(NumericMatrix m) ;
// ' @title Density
// ' @description Calculate network binary density.
// ' @param M a square adjacency matrix.
// ' @return double representing the network binary density.
// ' @details Binary network density is simply the ratio of existing links of a network in relation to all potential links.
// ' @author Sebastian Sosa, Ivan Puga-Gonzales.
// ' @references Sosa, S. (\emph{in press}). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
// ' @examples
// ' M=matrix(sample(c(0:10),100),ncol=10,nrow=10)
// ' met.met_density(M)
// ' @keyword internal
// [[Rcpp::export]]
double met_density(NumericMatrix M){
  int V=M.ncol();
  return sum(mat_col_sumsBinary(M))/(V*(V-1));
}
