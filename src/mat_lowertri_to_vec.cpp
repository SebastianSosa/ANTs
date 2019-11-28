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


// [[Rcpp::export]]
NumericVector mat_lowertri_to_vec(Rcpp::NumericMatrix& m, bool diag){
  int col=m.ncol();
  if(diag==false){
    int l=((col*col)/2)-(col/2);
    NumericVector vec(l);
    NumericVector& vec2=vec;
    int mem=0;
    
    for(int a=0;a<col;a++){
      
      Rcpp::NumericVector tmp=m(_,a);
      NumericVector& tmp2=tmp;
      tmp2.erase(tmp2.begin(),tmp2.begin()+a+1);
      
      std::copy(tmp2.begin(), tmp2.end(), vec2.begin()+mem);
      mem=mem+tmp.size();
    }
    return vec2;
  }
  else{
    int l=((col*col)/2)+(col/2);
    NumericVector vec(l);
    NumericVector& vec2=vec;
    int mem=0;
    
    for(int a=0;a<col;a++){
      Rcpp::NumericVector tmp=m(_,a);
      NumericVector& tmp2=tmp;
      tmp2.erase(tmp2.begin(),tmp2.begin()+a);
      
      std::copy(tmp2.begin(), tmp2.end(), vec2.begin()+mem);
      mem=mem+tmp.size();
    }
    return vec2;
  }

}