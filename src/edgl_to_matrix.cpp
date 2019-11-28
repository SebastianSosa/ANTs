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
NumericVector vec_match( CharacterVector x, CharacterVector y);

//' @title Edge list to matrix
//' @description Convert an edge list to a matrix.
//' @param df a data frame contening at leas three columns with the names: 'from','to' and 'weight', corresponding respectively to the individual emiting the beaheviour, the individual receving the beahviour, and the number of time this interaction occure.
//' @param sym a boolean if TRUE then the interactions are concider to be symetric.
//' @return A square adjacency matrix.
//' @details Adjacency matrix is one of the numerous representation of a social network.
//' @author Sebastian Sosa, Ivan Puga-Gonzales.
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix edgl_to_matrix(DataFrame df, bool sym) {

  CharacterVector from = df["from"];
  CharacterVector to = df["to"];
  NumericVector w = df["weight"];

  int s=from.size()*2;
  CharacterVector nodes(s);
  std::copy(from.begin(),from.end(),nodes.begin());
  std::copy(to.begin(), to.end(), nodes.begin() + from.size());
  nodes=unique(nodes);

  int N=nodes.size();
  NumericMatrix M(N,N);
  Rcpp::colnames(M)=nodes;
  Rcpp::rownames(M)=nodes;

  if(sym==false){
    for(int a=0;a<from.size();a++){
      
      CharacterVector f=CharacterVector::create(from(a));
      CharacterVector t=CharacterVector::create(to(a));
      
      NumericVector matchFrom=vec_match(nodes,f);
      NumericVector matchTo=vec_match(nodes,t);

      
      double ww=w[a];
      double oldW=M(matchFrom(0)-1,matchTo(0)-1);
      double newW=oldW+ww;
      
      M(matchFrom(0)-1,matchTo(0)-1)=newW;
      
    }
  }
  else{
    for(int a=0;a<from.size();a++){
      
      CharacterVector f=CharacterVector::create(from(a));
      CharacterVector t=CharacterVector::create(to(a));
      
      NumericVector matchFrom=vec_match(nodes,f);
      NumericVector matchTo=vec_match(nodes,t);
      
      double ww=w[a];
      
      double oldW1=M(matchFrom(0)-1,matchTo(0)-1);
      double newW1=oldW1+ww;
      
      double oldW2=M(matchTo(0)-1,matchFrom(0)-1);
      double newW2=oldW2+ww;
      
      M(matchFrom(0)-1,matchTo(0)-1)=newW1;
      M(matchTo(0)-1,matchFrom(0)-1)=newW2;
      
    }
  }

  return M;
}
