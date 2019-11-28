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
SEXP vec_merge(SEXP vec1, SEXP vec2);
DataFrame list_to_df(Rcpp::List lst);
//' @title Merge two data frames
//' @description Merge two data frames keeping the structure of the first one
//' @keywords internal
// [[Rcpp::export]]
DataFrame df_merge(DataFrame df1, DataFrame df2) {
  if(df1.size()!=df2.size()){Rcpp::stop("Data frames are not of the same length"); }

  List l(df1.size());
  for (int a=0; a<df1.size();a++){
    SEXP vec1=df1[a];
    SEXP vec2=df2[a];
    SEXP vec=vec_merge(vec1,vec2);
    l[a]=vec;
  }
  DataFrame result=list_to_df(l);
  result.names()=df1.names();
  return result;
}


