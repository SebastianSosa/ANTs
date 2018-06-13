// Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
//
// This file is part of Animal Network Toolkit (ANT).
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
Rcpp::NumericVector vec_match( Rcpp::CharacterVector x, Rcpp::CharacterVector y);
Rcpp::NumericVector extract_numericVector_elements(NumericVector y, NumericVector x);
Rcpp::CharacterVector vec_char_extract_IdValue(CharacterVector y, NumericVector x);

//' @title Empty group by individual matrix
//' @description Create an embty group by individual matrix according to the number of scans and individuals.
//' @param d a data frame 
//' @param col_scan an integer indicating the number of the column holding the different scans.
//' @param col_id an integer indicating the number of the column holding the different scans.
//' @return An empty group by individual matrix.
//' @details A group by individual matrix is a way to represent several data collection protocols such as group fellows or scans data collections. They also allow to compute easely several associations indexes.
//' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
//' @author Sebastian Sosa, Ivan Puga-Gonzales.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::NumericMatrix gbi_createEmpty(Rcpp::DataFrame d,int col_scan,
                              int col_id) {

  Rcpp::CharacterVector scansC=d[col_scan-1]; // scans vectors
  Rcpp::CharacterVector ids=d[col_id-1]; // id vector

  Rcpp::CharacterVector uScans=unique(scansC); // unique scans
  Rcpp::CharacterVector uIds=unique(ids);// unique scans

  Rcpp::NumericMatrix GBI(uScans.size(),uIds.size()); //empty gbi (row= scans, col=individuals)
  Rcpp::colnames(GBI)=uIds;
  Rcpp::rownames(GBI)=uScans;

  for(int a=0;a<uScans.size();a++){
    Rcpp::CharacterVector levela=Rcpp::as<std::string>(uScans[0]); //level a of scans
    Rcpp::NumericVector who=vec_match(scansC,levela); //Which lines match with this level of scan
    Rcpp::CharacterVector ids_levela= vec_char_extract_IdValue(ids,who); //which id are on this lines
    Rcpp::CharacterVector uIds_levela = unique(ids_levela); //extract the unics id
    Rcpp:NumericVector ok=vec_match(uIds_levela,uIds); //which of this unique are in the list of all the ID

    for(int b=0;b<uIds_levela.size();b++){
      int col=ok[b];
      GBI(a,col);
    }
  }
  return GBI;
}
