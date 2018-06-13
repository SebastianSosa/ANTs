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
//' @title Merge
//' @description Merge two vectors keeping their class
//' @keywords internal
// [[Rcpp::export]]
SEXP vec_merge(SEXP vec1, SEXP vec2){
  if(TYPEOF(vec1) != TYPEOF(vec2)){
    std::cout<<"vector 1 type: "<<TYPEOF(vec1)<<std::endl;
    std::cout<<"vector 2 type: "<<TYPEOF(vec2)<<std::endl;
    Rcpp::stop("Vectors are not of the same type"); 
  }
  switch( TYPEOF(vec1) ) {
  /* logical vectors (10)*/
  case LGLSXP:{
    LogicalVector v1=vec1;
    LogicalVector v2=vec2;
    LogicalVector vec(v1.size()+v2.size());
    std::copy(v1.begin(), v1.end(), vec.begin());
    std::copy(v2.begin(), v2.end(), vec.begin()+v1.size());
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
    IntegerVector v2=vec2;

    bool test1=Rf_isNull(v1.attr("class"));
    bool test2=Rf_isNull(v2.attr("class"));
    if(test1==TRUE && test2==TRUE){
      IntegerVector v1=vec1;
      IntegerVector v2=vec2;
      IntegerVector vec(v1.size()+v2.size());
      std::copy(v1.begin(), v1.end(), vec.begin());
      std::copy(v2.begin(), v2.end(), vec.begin()+v1.size());
      return vec;
    }
    else{
      std::string r1=v1.attr("class");
      std::string r2=v1.attr("class");
      if(r1 != r2){Rcpp::stop("Vectors are not of the class type"); }
      if(r1=="integer"){
        IntegerVector v1=vec1;
        IntegerVector v2=vec2;
        IntegerVector vec(v1.size()+v2.size());
        std::copy(v1.begin(), v1.end(), vec.begin());
        std::copy(v2.begin(), v2.end(), vec.begin()+v1.size());
        return vec;
      }
      if(r1=="hexmode"){
        IntegerVector v1=vec1;
        IntegerVector v2=vec2;
        IntegerVector vec(v1.size()+v2.size());
        std::copy(v1.begin(), v1.end(), vec.begin());
        std::copy(v2.begin(), v2.end(), vec.begin()+v1.size());
        vec.attr("class") = "hexmode";
        return vec;
      }
      if(r1=="octmode"){
        IntegerVector v1=vec1;
        IntegerVector v2=vec2;
        IntegerVector vec(v1.size()+v2.size());
        std::copy(v1.begin(), v1.end(), vec.begin());
        std::copy(v2.begin(), v2.end(), vec.begin()+v1.size());
        vec.attr("class") = "octmode";
        return vec;
      }
      if(r1=="factor"){
        CharacterVector v1=vec1;
        CharacterVector v2=vec2;
        CharacterVector vec3(v1.size()+v2.size());
        std::copy(v1.begin(), v1.end(), vec3.begin());
        std::copy(v2.begin(), v2.end(), vec3.begin()+v1.size());
        CharacterVector levs = sort_unique(vec3);
        IntegerVector vec = match(vec3, levs);
        vec.attr("levels") = levs;
        vec.attr("class") = "factor";
        return vec;
      }
    }
  }
  /* real variables(14) */
  case REALSXP:{
    NumericVector v1=vec1;
    NumericVector v2=vec2;
    NumericVector vec(v1.size()+v2.size());
    std::copy(v1.begin(), v1.end(), vec.begin());
    std::copy(v2.begin(), v2.end(), vec.begin()+v1.size());
    return vec;
  }
  /* string vectors(16) */
  case STRSXP:{
    StringVector  v1=vec1;
    StringVector  v2=vec2;
    StringVector  vec(v1.size()+v2.size());
    std::copy(v1.begin(), v1.end(), vec.begin());
    std::copy(v2.begin(), v2.end(), vec.begin()+v1.size());
    return vec;
  }
  }
  Rcpp::stop("Input vectors are not Vectors are not string,real,integer or logical"); 
}