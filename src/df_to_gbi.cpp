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
NumericVector vec_match( CharacterVector x, CharacterVector y);

//' @title Group by individual matrix.
//' @description Convert a data frame into a group by individual matrix. 
//' @param d a data frame.
//' @param col_scan an integer indicating the column number holding the information of the scans.
//' @param col_id an integer indicating the column number holding the individuals observations.
//' @param uIds a character vector holding all the ids of the individuals.
//' @param uScans a character vector holding all the ids of the scans. 
//' @return A group by individual matrix.
//' @author Sebastian Sosa, Ivan Puga-Gonzales
//' @keywords internal
// [[Rcpp::export]]
Rcpp::NumericMatrix df_to_gbi(Rcpp::DataFrame d,int col_scan,int col_id,
                              Rcpp::CharacterVector uIds,// unique scans
                              Rcpp::CharacterVector uScans) {// unique scans
  Rcpp::CharacterVector scansC=d[col_scan-1]; // scans vectors
  Rcpp::CharacterVector ids=d[col_id-1]; // id vector

  Rcpp::NumericMatrix GBI(uScans.size(),uIds.size()); //empty gbi (row= scans, col=individuals)
  Rcpp::colnames(GBI)=uIds;
  Rcpp::rownames(GBI)=uScans;

  ///loop throuhg every single group
  for(int a=0; a< GBI.nrow();a++){
    Rcpp::CharacterVector levela=as<std::string>(uScans(a));
    Rcpp::NumericVector who=vec_match(scansC,levela);

    //loop through the individuals found in the group
    Rcpp::CharacterVector idx;
    for(int b=0; b<who.size();b++){
      int index=(who(b) - 1); // Indices empezando de 1, necesita restar 1 para uso en Cpp
      idx.push_back(ids(index));
    }
    idx=unique(idx);// get the IDs of the individuals in the group without repetition
    Rcpp::NumericVector Indexes=vec_match(uIds,idx);// match the ids found in the group with the ids
      //fill the gbi according to the ids found in group
      for(int c=0; c<Indexes.size();c++){
        int index2=(Indexes(c) - 1);// Indices empezando de 1, necesita restar 1 para uso en Cpp
        GBI(a,index2)=1;// INDICES MUST BE BETWEEN () NOT []!!!!!!
      }
  }
return GBI;
}

