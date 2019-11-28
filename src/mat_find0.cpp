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
#include <math.h> 
// [[Rcpp::depends(RcppArmadillo)]]
//' @title Find zeros in a Matrix.
//' @description Find empty cells in a matrix.
//' @param m a matrix.
//' @return A data frame with two columns the first one representing the row id and the second one trepresenting the column id of the zero cell.
//' @author Sebastian Sosa, Ivan Puga-Gonzales.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::DataFrame mat_find0(arma::mat m) {
  arma::uvec move = find(m==0); // Find IDs & obtain values
  int nrow=m.n_rows+1;
  int max_elements= move.n_rows;
  Rcpp::NumericVector row_id(max_elements);
  Rcpp::NumericVector col_id(max_elements);
  for (int a=0; a<max_elements; a++){
    double element=move[a]+1;
    double c=element/nrow;
    int element_col=ceil(c);
    double v=floor(c);
    if(c-v==0){
      int element_row=nrow;
      row_id[a]=element_row;
      col_id[a]=element_col;
    }
    else{
      int element_row=ceil((c-v)*nrow);
      row_id[a]=element_row;
      col_id[a]=element_col;
    }

  }
  Rcpp::DataFrame df =
    Rcpp::DataFrame::create(Rcpp::Named("row_id")=row_id,Rcpp::Named("col_id")=col_id);

  arma::vec pick = arma::linspace<arma::vec>(1,1,2);
  return df;
}
