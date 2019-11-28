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
IntegerVector  perm_vec_factor(CharacterVector vec);
//' @title Vector sample
//' @description Sample vetor keeping his class
//' @keywords internal
// [[Rcpp::export]]
SEXP vec_sample(SEXP vec1,int length,bool replace){
  switch( TYPEOF(vec1) ) {
  /* logical vectors (10)*/
  case LGLSXP:{
    LogicalVector vec=vec1;
    vec=Rcpp::sample(vec,length,replace);
    return vec;
  }
    /* integer vectors with different clases (13):
    * hexmode
    * integer
    * factor
    * octomode
    */
  case INTSXP:{
    
    IntegerVector v1=vec1;
    bool test1=Rf_isNull(v1.attr("class"));
    if(test1==TRUE){
      IntegerVector vec=vec1;
      vec=Rcpp::sample(vec,length,replace);
      //vec.attr("class") = "integer";
      return vec;
    }
    else{
      
      std::string r1=v1.attr("class");
      if(r1=="integer"){
        IntegerVector vec=vec1;
        vec=Rcpp::sample(vec,length,replace);
        //vec.attr("class") = "integer";
        return vec;
      }
      
      if(r1=="factor"){
        CharacterVector vec=vec1;
        vec=Rcpp::sample(vec,length,replace);
        CharacterVector levs = sort_unique(vec);
        IntegerVector result = match(vec, levs);
        result.attr("levels") = levs;
        result.attr("class") = "factor";
        return result;
      }
    }
    
  }
    /* real variables(14) */
  case REALSXP:{
    
    NumericVector vec=vec1;
    vec=Rcpp::sample(vec,length,replace);
    return vec;
  }
    /* string vectors(16) */
  case STRSXP:{
    StringVector  vec=vec1;
    vec=Rcpp::sample(vec,length,replace);
    return vec;
  }
  }
  Rcpp::stop("Input vectors are not Vectors are not string,real,integer or logical"); 
}
