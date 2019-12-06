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
using namespace std;

//' @title Merge columns of a List of data frames.
//' @description perm_dataStraem_focal subfunction to concatenate the columns of alters of a list of data frames.
//' @param df a data frame to know the total size of the concatenated vector.
//' @param ldf a list of data frames in wich extarct a single column.
//' @param col an integer indicating wich column of the data frames extract. Is a R indexation.
//' @return A character vector.
//' @author Sebastian Sosa, Ivan Puga-Gonzales.
//' @keywords internal
// [[Rcpp::export]]
SEXP listDf_merge_single_column(Rcpp::DataFrame df, Rcpp::List ldf,int col){
  Rcpp::CharacterVector alters=df[col-1];
  int length_alters=alters.size();
  Rcpp::List tmpLdf=clone(ldf);
  Rcpp::CharacterVector vec(length_alters);

  int index = 0;
  for(int b=0;b<ldf.size();b++){
    //Rcpp::Rcout<<"Merging vectors: "<<index<<std::endl;
    Rcpp::DataFrame tmpdf=Rcpp::as<Rcpp::DataFrame>(tmpLdf[b]); // selecting  element of the list
    Rcpp::CharacterVector tmpAlters=tmpdf[col-1]; //slecting column
    std::copy(tmpAlters.begin(), tmpAlters.end(), vec.begin() + index);
    index += tmpAlters.size(); //Updating vec according to the size of tmpAlters paste
  }
  return vec;
}
