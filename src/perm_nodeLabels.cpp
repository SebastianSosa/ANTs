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
SEXP vec_sample_all(SEXP vec1);
//' @title Node label permutations.
//' @description Post-network permutations of the type 'node labels'.
//' @param df a data frame.
//' @param label a numeric vector indicating the number of individuals.
//' @param nperm an integer indicating the number of permutations to perform.
//' @param progress a boolean indicating if you wich to see the progression of the permutations.
//' @return A list of data frames, each ones holding a different permutation.
//' @details  Node label permutations is a post-network permutations approach that swap the labels of the nodes (e.g. degree). It is use on direct interactions behaviours.
//' @author Sebastian Sosa,Ivan Puga-Gonzales.
//' @references Farine, D. R. (2017). A guide to null models for animal social network analysis. Methods in Ecology and Evolution.
//' @references Sosa, S. (\emph{in press}). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
//' @keywords internal
// [[Rcpp::export]]

Rcpp::List perm_nodeLabels(Rcpp::DataFrame df,Rcpp::NumericVector label,int nperm, bool progress){
  Rcpp::List list_Df(nperm+1);
  list_Df[0]=clone(df);
  
  if(progress==TRUE){
    for(int a=1;a<nperm+1;a++){
      // Print permutations progress
       Rcpp::Rcout <<"\r"<<"Permutation: "<<a;
       Rcpp::Rcout.flush();
      
      Rcpp::DataFrame df2=clone(df);
      for(int b=0;b<label.size();b++){
        int col=label[b]-1;
        SEXP vec=df2[col];
        SEXP samp= vec_sample_all(vec);
        df2[col]=samp;
        
      }
      df2.attr("permutation") = a;
      list_Df[a]=clone(df2);
    }
     Rcpp::Rcout<<"\n"<<std::endl;
  }
  else{
    for(int a=1;a<nperm+1;a++){
      Rcpp::DataFrame df2=clone(df);
      for(int b=0;b<label.size();b++){
        int col=label[b]-1;
        SEXP vec=df2[col];
        SEXP samp= vec_sample_all(vec);
        df2[col]=samp;
        
      }
      df2.attr("permutation") = a;
      list_Df[a]=clone(df2);
    }
  }
  
  return list_Df;
}
