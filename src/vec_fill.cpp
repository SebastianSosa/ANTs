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
//' @title Merge
//' @description Merge two vectors keeping their class
//' @keywords internal
// [[Rcpp::export]]
SEXP vec_fill(SEXP vec1, SEXP vec2, int x){
  
  if(TYPEOF(vec1) != TYPEOF(vec2)){
    std::cout<<"\n";
    std::cout<<"vector 1 type: "<<TYPEOF(vec1);
    std::cout<<"\n";
    std::cout<<"vector 2 type: "<<TYPEOF(vec2);
    std::cout<<"\n"<<std::endl;
    
    Rcpp::stop("Vectors are not of the same type"); 
  }
  
  switch( TYPEOF(vec1) ) {
  /* logical vectors (10)*/
  case LGLSXP:{
    LogicalVector v1=vec1;
    LogicalVector v2=vec2;
    std::copy(v2.begin(), v2.end(), v1.begin()+x-1);
    return v1;
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
      std::copy(v2.begin(), v2.end(), v1.begin()+x-1);
      return v1;
    }
    else{
      std::string r1=v1.attr("class");
      std::string r2=v1.attr("class");
      if(r1 != r2){Rcpp::stop("Vectors are not of the class type"); }
      if(r1=="integer"){
        IntegerVector v1=vec1;
        IntegerVector v2=vec2;
        std::copy(v2.begin(), v2.end(), v1.begin()+x-1);
        return v1;
      }
      if(r1=="hexmode"){
        IntegerVector v1=vec1;
        IntegerVector v2=vec2;
        std::copy(v2.begin(), v2.end(), v1.begin()+x-1);
        v1.attr("class") = "hexmode";
        return v1;
      }
      if(r1=="octmode"){
        IntegerVector v1=vec1;
        IntegerVector v2=vec2;
        std::copy(v2.begin(), v2.end(), v1.begin()+x-1);
        v1.attr("class") = "octmode";
        return v1;
      }
      if(r1=="factor"){
        CharacterVector v1=vec1;
        CharacterVector v2=vec2;
        std::copy(v2.begin(), v2.end(), v1.begin()+x-1);
        CharacterVector levs = sort_unique(v1);
        levs=na_omit(levs);
        IntegerVector vec2 = match(v1, levs);
        vec2.attr("levels") = levs;
        vec2.attr("class") = "factor";
        return vec2;
      }
    }
  }
    
    /* real variables(14) */
  case REALSXP:{
    NumericVector v1=vec1;
    NumericVector v2=vec2;
    std::copy(v2.begin(), v2.end(), v1.begin()+x-1);
    return v1;
  }
    
    /* string vectors(16) */
  case STRSXP:{
    StringVector  v1=vec1;
    StringVector  v2=vec2;
    std::copy(v2.begin(), v2.end(), v1.begin()+x-1);
    return v1;
  }
  }
  Rcpp::stop("Input vectors are not of type string,real,integer or logical."); 
}