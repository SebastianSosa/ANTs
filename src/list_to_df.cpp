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

//' @title List to data frame.
//' @description Convert a list of vectors into a data frame
//' @param lst a list.
//' @return A data frame with the same number of columns as the list length.
//' @keywords internal
// [[Rcpp::export]]
List list_to_df(List a) {
  List returned_frame = clone(a);
  GenericVector sample_row = returned_frame(0);

  StringVector row_names(sample_row.length());
  for (int i = 0; i < sample_row.length(); ++i) {
    //char name[11];
    //sprintf(&(name[0]), "%d", i);
    row_names(i) = i;
  }
  returned_frame.attr("row.names") = row_names;

  StringVector col_names(returned_frame.length());
  for (int j = 0; j < returned_frame.length(); ++j) {
    //char name[11];
    //sprintf(&(name[0]), "X.%d", j);
    col_names(j) = j;
  }
  returned_frame.attr("names") = col_names;
  returned_frame.attr("class") = "data.frame";

  return returned_frame;
}
