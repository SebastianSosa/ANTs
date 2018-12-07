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

#include <Rcpp.h>
using namespace Rcpp;
NumericVector vec_unmatch_indexcc( CharacterVector x, CharacterVector y);
NumericMatrix edgl_to_matrix(DataFrame df, bool sym);
//' @title Data Stream Focal Sampling cumulative permutations without control factor.
//' @description Pre-network cumulative permutation on association data of  Focal Sampling type without control factor.
//' @param df a data frame.
//' @param ldf2 a list of data frames split by individuals focals.
//' @param nperm an integer indicating the number of permutations to perform.
//' @param id_obs a numeric vector with the number of different ids.
//' @param col_focal an integer indicating the column holding the id of the focal.
//' @param col_alters an integer indicating the column holding the id of the alters.
//' @param lengthList an integer vector range from 0 to length of the list (c++ indexation).
//' @return A list of data frames, each ones holding a different permutation.
//' @details  Data stream permutations is a pre-network permutations approach. It is use on association data based on the gambit of the group. This permutations functunction is made for data collected of the type of 'focal sampling' and without control factors
//' @author Sebastian Sosa,Ivan Puga-Gonzales.
//' @references Farine, D. R. (2017). A guide to null models for animal social network analysis. Methods in Ecology and Evolution.
//' @references Sosa, S. (\emph{in press}). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List  redo_perm_dataStream_focal(Rcpp::DataFrame df,Rcpp::List ldf1, int nperm,
                                  int col_focal,int col_alters,
                                  Rcpp::IntegerVector lengthList) {
  
  Rcpp::List result(2);
  Rcpp::DataFrame newDf=Rcpp::clone(df);
  StringVector newAlters=newDf[col_alters-1];
  
    for(int a=0; a<nperm; a++){
      
      std::string  unique_focal1= "a"; //Name of focal 1 pick
      std::string  unique_focal2= "a"; //Name of focal 2 pick
      
      Rcpp::StringVector  alters_id1; //will hold unique alters of  focal 1
      Rcpp::StringVector  alters_id2; //will hold unique alters of  focal 2
      
      std::string  unique_alter_id1 = "a"; //will hold one laters of alters_id1
      std::string  unique_alter_id2 = "a"; //will hold one laters of alters_id2
      
      Rcpp::NumericVector position_unique_alters_id1; //Position of the unique alters of  focal 1
      Rcpp::NumericVector position_unique_alters_id2; //Position of the unique alters of  focal 2
      
      Rcpp::IntegerVector dfSelected(2); //will hold two random number slected between c(1:length(ldf2))
      
      Rcpp::NumericVector pick1;
      Rcpp::NumericVector pick2;
      
      int ct=0;
      while(unique_alter_id1==unique_alter_id2){
        ct=ct+1;
        if (a % 1024  == 0) Rcpp::checkUserInterrupt();
        // Selection of two data frames randomly
        dfSelected=Rcpp::sample(lengthList,2, false);
        Rcpp::DataFrame dfSelected1=ldf1[dfSelected[0]];
        Rcpp::DataFrame dfSelected2=ldf1[dfSelected[1]];
        
        // Extracting focals names
        Rcpp::StringVector ff1=dfSelected1[col_focal-1];
        unique_focal1=ff1[0];
        Rcpp::StringVector ff2=dfSelected2[col_focal-1];
        unique_focal2=ff2[0];
        
        //checking that focal one is different from focal 2
        if(unique_focal2!=unique_focal1){
          // Extracting alters of each focals
          alters_id1=dfSelected1[col_alters-1];
          alters_id2=dfSelected2[col_alters-1];
          
          // Which alters of focal 1 is not in alters of focal 2 and vis et versa
          position_unique_alters_id1=vec_unmatch_indexcc(alters_id1,alters_id2);
          position_unique_alters_id2=vec_unmatch_indexcc(alters_id2,alters_id1);
          
          // Checking if both of the focal have unique alter
          if(position_unique_alters_id1.size()>0 && position_unique_alters_id2.size()>0){
            // Selecting randomly one unique alter of focal 1
            pick1=Rcpp::sample(position_unique_alters_id1,1,false);
            
            // Selecting randomly one unique alter of focal 2
            pick2=Rcpp::sample(position_unique_alters_id2,1,false);
            
            // Checking if the unique alter of focal 1 is different from focal two and vice versa
            if(alters_id1[pick1(0)]!=unique_focal2 && alters_id2[pick2(0)]!=unique_focal1){
              unique_alter_id1=alters_id1[pick1(0)]; // selecting the first one of id1 unique alter (less one becaus umatch function is made for R thus return index +1)
              unique_alter_id2=alters_id2[pick2(0)]; // selecting the first one of id2 unique alter
            }
          }
        }
      }

      Rcpp::DataFrame id1=ldf1[dfSelected(0)];
      Rcpp::DataFrame id2=ldf1[dfSelected(1)];
      
      alters_id1[pick1(0)]=unique_alter_id2;
      alters_id2[pick2(0)]=unique_alter_id1;

      id1[col_alters-1]=alters_id1;
      id2[col_alters-1]=alters_id2;
      
      ldf1[dfSelected(0)]=id1;
      ldf1[dfSelected(1)]=id2;
      
      IntegerVector rowFocal1=id1["nrow"];
      IntegerVector rowFocal2=id2["nrow"];
      
      // row number of unique alter of focal one
      int nrowU1=rowFocal1[pick1(0)];
      int nrowU2=rowFocal2[pick2(0)];
      
      newAlters[nrowU1-1]=unique_alter_id2;
      newAlters[nrowU2-1]=unique_alter_id1;
      
      newDf[col_alters-1]=newAlters;
  }
    
  result[0]=newDf;
    
  // Converting the data frame into a matrix
  int N=newDf.nrows();
  NumericVector W(N,1.);
  //int ncol=newDf.size();
  DataFrame to_convert=DataFrame::create(_["from"]= newDf[col_focal-1], _["to"]= newDf[col_alters-1], _["weight"]= W); 
  NumericMatrix M=edgl_to_matrix(to_convert,true);
  CharacterVector names = colnames(M);
  result[1]=M;
    
  return(result);
}