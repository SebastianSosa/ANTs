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
List list_to_df(Rcpp::List lst);
SEXP vec_merge(SEXP vec1, SEXP vec2);
//' @title Merge list of data frames 
//' @description Merge a list of  data frames keeping the structure of the first one
//' @keywords internal
// [[Rcpp::export]]
List ldf_merge(List ldf) {
  DataFrame d= Rcpp::as<Rcpp::DataFrame>(ldf[0]);
  List ldf2(d.size());
  
  for(int a=0;a<d.size();a++){
    SEXP vec=d[a];
    //Rcpp::Rcout<<"col id: "<<a<<std::endl;
    
    for(int b=1;b<ldf.size();b++){
      //Rcpp::Rcout<<"data frame is: "<<b<<std::endl;
      DataFrame d2=Rcpp::as<Rcpp::DataFrame>(ldf[b]);
      SEXP vec2=d2[a];
      vec=vec_merge(vec,vec2);
    }
    
    ldf2[a]=vec;
  }
  List result=list_to_df(ldf2);
  result.names()=d.names();
  return result;
}
