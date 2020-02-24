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
arma::mat assoc_mat (arma::mat Mgbi, std::string method, bool return_denom = false);
arma::rowvec assoc_mat_one_id (arma::mat Mgbi,int id, std::string method);
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
//' @title Data Stream gambit of the group cumulative permutations without control factor.
//' @description Cumulative pre-network permutation on association data of gambit of the group type without control factor.
//' @param M a square adjacency matrix.
//' @param nperm an integer indicating the number of permutations to perform.
//' @details  Data stream permutations is a pre-network permutations approach. It is use on association data based on the gambit of the group. This permutations functunction is made for  data collected of the type of 'gambit of the group' and without control factors
//' @return A list of Group By Individual matrices according to each scans perform.
//' @author Ivan Puga-Gonzales, Sebastian Sosa.
//' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
//' @references Farine, D. R. (2017). A guide to null models for animal social network analysis. Methods in Ecology and Evolution.
//' @references Sosa, S. (\emph{in press}). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
//' @keywords internal
// [[Rcpp::export]]

Rcpp::List redo_perm_dataStream_1(arma::mat M, int nperm, std::string method) {
  
  Rcpp::List result(2);
  for (int a=0;a<nperm;a++){
    
    // Finding non empty cells in the matrix
    arma::uvec move = find(M==1); // Find elements=0
    arma::umat idx  = ind2sub( size(M), move ); // returns row (1) and columns (2) of ids in GBI
    
    // Parametrization
    int pick1=0;
    int pick2=0;
    
    arma::uvec id1_info(2);
    arma::uvec id2_info(2);
    id1_info=idx.col(pick1);
    id2_info=idx.col(pick2);
    
    int nscan=idx.n_cols;
    int scan_id1=id1_info(0);// Row information of id1 (the group)
    int scan_id2=id2_info(0);//Row information of id2 (the individual in the group)
    int id1=id1_info(1);// Col information of id1 (the individual in the group)
    int id2=id2_info(1);// Col information of id2 (the individual in the group)
    
    Rcpp::IntegerVector scans=Rcpp::seq(0,nscan-1);
    
    // Find permutable individuals
    while(id1==id2 || 
          scan_id1==scan_id2 ||
          M(scan_id2,id1)==1 || 
          M(scan_id1,id2)==1){
      Rcpp::IntegerVector pick3 = Rcpp::sample(scans, 2, false);
      pick1=pick3(0);
      pick2=pick3(1);
      id1_info=idx.col(pick1);
      id2_info=idx.col(pick2);
      scan_id1=id1_info(0);// Row information of id1 (the group)
      scan_id2=id2_info(0);//Row information of id2 (the individual in the group)
      id1=id1_info(1);// Col information of id1 (the individual in the group)
      id2=id2_info(1);// Col information of id2 (the individual in the group)
    }
    
    // Modify their values in gbi
    M(scan_id2,id1)=1;
    M(scan_id1,id2)=1;
    
    M(scan_id1,id1)=0;
    M(scan_id2,id2)=0;
  }
  result[0]=M;
  result[1]=assoc_mat(M,method);
  return result;
}
