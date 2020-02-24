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
arma::mat assoc_mat (arma::mat Mgbi, std::string method, bool return_denom = false);
arma::rowvec assoc_mat_one_id (arma::mat Mgbi,int id, std::string method);
//#include <RcppArmadilloExtensions/sample.h>
#include <iostream>
//' @title Data Stream gambit of the group Permutations with control factor.
//' @description Pre-network permutation on association data of gambit of the group type with control factor. 
//' @param M a square adjacency matrix.
//' @param nperm an integer indicating the number of permutations to perform.
//' @param CumSizesGbis ???
//' @param progress a boolean indicating if you wich to see the progression of the permutations.
//' @return A list of Group By Individual matrices according to each scans perform.
//' @details  Data stream permutations is a pre-network permutations approach. It is use on association data based on the gambit of the group. This permutations functunction is made for  data collected of the type of 'gambit of the group' and without control factors
//' @author Ivan Puga-Gonzales, Sebastian Sosa.
//' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
//' @references Farine, D. R. (2017). A guide to null models for animal social network analysis. Methods in Ecology and Evolution.
//' @references Sosa, S. (\emph{in press}). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List perm_dataStream_ControlFactor(Rcpp::List GBIList,
                                         arma::mat M,
                                         int nperm,
                                         Rcpp::IntegerVector GBIIndexes, 
                                         Rcpp::IntegerVector CumSizesGbis, 
                                         bool progress,
                                         std::string method ){
  //int Size = GBIList.size();
  Rcpp::List list_assoc_mat(nperm+1);// NEW LINE
  //Rcpp::List list_gbi(nperm+1);// list to hold permutated gbis //OLD LINE 
  // CREATE ASSOCIATION MATRIX
  arma::mat tmp=assoc_mat(M,method);// NEW LINE
  list_assoc_mat[0]=tmp;// NEW LINE
  //list_gbi[0]=M;// fill in with the first gbi (the observe gbi) //OLD LINE 
  int Current_perm = 1;
  // CHANGE BUG FIX 20190320
  //Rcpp::IntegerVector GBIS=Rcpp::seq(0,(GBIList.size()-1));// vector to sample through gbis

  while(Current_perm < (nperm+1)){
    if(progress==TRUE){
      Rcpp::Rcout<<"\r"<<"Permutation: "<<Current_perm;
      Rcpp::Rcout.flush();
    }
    Rcpp::IntegerVector GbiIndex = Rcpp::sample(GBIIndexes,1); // pick a gbi randomly // CHANGE BUG FIX 20190320 // argument in sample change from GBIS to GBIIndexes
    //Rcpp::Rcout<< "GbiIndex" <<std::endl;
    //Rcpp::Rcout<< GbiIndex <<std::endl;
    arma::mat GBI_M= GBIList[GbiIndex(0)]; //extract gbi form the list

    // Finding non empty cells in the matrix
    arma::uvec move = find(GBI_M==1); // Find elements=0
    arma::umat idx  = ind2sub(size(GBI_M), move ); // returns row (1) and columns (2) of ids in GBI
    //arma::uvec move = find(GBI_M==0); // Find elements equal to zero
    //arma::umat idx  = ind2sub(size(GBI_M),move);//Returns matrix  2xSizeMove row1=GroupID row2=IndID

    int pick1=0;// individual 1
    int pick2=0;// indivdiual 2
    arma::uvec id1_info(2);//vector of size 2 to hold info of ind 1
    arma::uvec id2_info(2);//vector of size 2 to hold info of ind 2
    id1_info=idx.col(pick1);// fills GroupID and IndID of pick 1
    id2_info=idx.col(pick2);// fills GroupID and IndID of pick 2

    int nscan=idx.n_cols;//
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
      //Rcpp::Rcout<< "GbiIndex "<<GbiIndex <<std::endl;
      //Rcpp::Rcout<< "id1 "<< id1 <<std::endl;
      //Rcpp::Rcout<< "scan_id1 "<< scan_id1 <<std::endl;
      //Rcpp::Rcout<< "id2 " << id2<<std::endl;
      //Rcpp::Rcout<< "scan_id2 "<< scan_id2 <<std::endl;
      //Rcpp::Rcout<< "WhileIndex" << WhileIndex<<std::endl;
      // Maximum number of while loops = 100
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
      //Rcpp::Rcout<< "GbiIndex "<<GbiIndex <<std::endl;
      //Rcpp::Rcout<< "id1 "<< id1 <<std::endl;
      //Rcpp::Rcout<< "scan_id1 "<< scan_id1 <<std::endl;
      //Rcpp::Rcout<< "id2 " << id2<<std::endl;
      //Rcpp::Rcout<< "scan_id2 "<< scan_id2 <<std::endl;
      // PASTE MODIFIED PARTIAL GBI INTO ALL GBI
      int start = CumSizesGbis(GbiIndex(0));
      int GBI_Mrows = GBI_M.n_rows;
      for (int j = 0; j < GBI_Mrows; j++) {
        M.row(start) = GBI_M.row(j);
        start += 1;
      }
      Rcpp::NumericMatrix y = Rcpp::wrap(GBI_M);// Convert arma matrix type to Rcpp matrix type
      GBIList[GbiIndex(0)] = y;
      
      // Modify the association matrix
      arma::rowvec new_assoc_id1=assoc_mat_one_id(M,id1,method);// NEW LINE
      arma::rowvec new_assoc_id2=assoc_mat_one_id(M,id2,method);// NEW LINE
      tmp.row(id1)=new_assoc_id1;// NEW LINE
      tmp.row(id2)=new_assoc_id2;// NEW LINE
      tmp.col(id1)=arma::vectorise(new_assoc_id1);// NEW LINE
      tmp.col(id2)=arma::vectorise(new_assoc_id2);// NEW LINE
      tmp.diag().zeros();// NEW LINE
      
      list_assoc_mat[Current_perm]=tmp;// NEW LINE
      //list_gbi[Current_perm]=Rcpp::wrap(M); //OLD LINE 
      Current_perm += 1;
    }// end if condition
  }// end permutation
  Rcpp::Rcout<<"\n"<<std::endl;
  return list_assoc_mat;// NEW LINE
  //return list_gbi; //OLD LINE 
}
