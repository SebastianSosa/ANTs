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


// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
arma::mat assoc_mat (arma::mat Mgbi, std::string method);
arma::rowvec assoc_mat_one_id (arma::mat Mgbi,int id, std::string method);
//#include <RcppArmadilloExtensions/sample.h>
#include <iostream>
//' @title Data Stream gambit of the group cumulative permutations with control factor.
//' @description Cumulative pre-network permutation on association data of gambit of the group type with control factor. 
//' @param GBIList a list of gbis split according to the control(s) factors.
//' @param nperm an integer indicating the number of permutations to perform.
//' @param CumSizesGbis ???
//' @details  Data stream permutations is a pre-network permutations approach. It is use on association data based on the gambit of the group. This permutations functunction is made for  data collected of the type of 'gambit of the group' and without control factors
//' @author Ivan Puga-Gonzales, Sebastian Sosa.
//' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
//' @references Farine, D. R. (2017). A guide to null models for animal social network analysis. Methods in Ecology and Evolution.
//' @references Sosa, S. (\emph{in press}). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
//' @keywords internal
// [[Rcpp::export]]

Rcpp::List redo_perm_dataStream_ControlFactor_scd(Rcpp::List GBIList,int nperm,std::string method){
  
  Rcpp::List result(2);
  
  int Current_perm = 1;
  Rcpp::IntegerVector GBIS=Rcpp::seq(0,(GBIList.size()-1));
  
  while(Current_perm < (nperm)){
    // pick a gbi randomly
    Rcpp::IntegerVector GbiIndex = Rcpp::sample(GBIS,1); 
    
    //extract gbi form the list
    arma::mat GBI_M= GBIList[GbiIndex(0)];
    
    // Finding non empty cells in the matrix
    arma::uvec move = find(GBI_M==1);
    arma::umat idx  = ind2sub(size(GBI_M), move ); // returns row (1) and columns (2) of ids in GBI
    
    int pick1=0;// individual 1
    int pick2=0;// indivdiual 2
    
    arma::uvec id1_info(2);//vector of size 2 to hold info of ind 1
    arma::uvec id2_info(2);//vector of size 2 to hold info of ind 2
    
    id1_info=idx.col(pick1);// fills GroupID and IndID of pick 1
    id2_info=idx.col(pick2);// fills GroupID and IndID of pick 2
    
    int nscan=idx.n_cols;
    int scan_id1=id1_info(0);// Group of id1 (row in matrix idx)
    int scan_id2=id2_info(0);// Group of id2 (row in matrix idx)
    int id1=id1_info(1);// ID of id1 (col in matrix idx)
    int id2=id2_info(1);// ID of id2 (col in matrix idx)
    
    Rcpp::IntegerVector scans=Rcpp::seq(0,nscan-1);
    int WhileIndex = 0;
    bool SwapFound = true;
    while(id1==id2 || // same individuals
          scan_id1==scan_id2 || // individuals in same groups
          GBI_M(scan_id2,id1)==1 || // individual 1 not present in group of ind 2
          GBI_M(scan_id1,id2)==1){ // individual 2 not present in group of ind 1
      
      Rcpp::IntegerVector pick3 = Rcpp::sample(scans, 2, false);
      pick1=pick3(0); // assign new ind 1
      pick2=pick3(1); // assign new ind 2
      id1_info=idx.col(pick1);// CHECK THIS IS CORRECT
      id2_info=idx.col(pick2);// CHECK THIS IS CORRECT
      scan_id1=id1_info(0);// Group of id1 (row in matrix idx)
      scan_id2=id2_info(0);//Group of id2 (row in matrix idx)
      id1=id1_info(1);// ID of id1 (col in matrix idx)
      id2=id2_info(1);// ID of id2 (col in matrix idx)
      WhileIndex += 1;
      
      if (WhileIndex == 100){
        SwapFound = false;
        break;
      }
    }// end while loop
    if (SwapFound == true){
      GBI_M(scan_id2,id1)=1;
      GBI_M(scan_id1,id2)=1;
      GBI_M(scan_id1,id1)=0;
      GBI_M(scan_id2,id2)=0;
      

      GBIList[GbiIndex(0)]=GBI_M;
      Current_perm += 1;
    }// end if condition
  }// end permutation
  return GBIList;
}
