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
DataFrame ldf_merge(List ldf);
SEXP vec_sample_all(SEXP vec1);
//' @title Node lable permutation with random factors
//' @description Perorm node label permutation on list of data frames
//' @keywords internal
// [[Rcpp::export]]
List perm_nl_rf(List ldf, NumericVector lables, int nperm, bool progress) {
  List pldf(nperm+1);
  pldf[0]=ldf_merge(ldf);
  if(progress==TRUE){
    for(int a=1; a<nperm+1;a++){
      List ldf2=clone(ldf);
      std::cout<<"\r"<<"permutation: "<<a;
      for(int b=0;b<ldf.size();b++){
        DataFrame d=ldf2[b];
        DataFrame df=clone(d);
        for(int c=0;c<lables.size();c++){
          int n=lables[c]-1;
          SEXP vec=df[n];
          SEXP newvec=vec_sample_all(vec);
          df[n]=newvec;
        }
        ldf2[b]=df;
      }
      DataFrame tmp=ldf_merge(ldf2);
      tmp.attr("permutation") = a;
      pldf[a]=tmp;
    }
    std::cout<<"\n"<<std::endl;
  }
  else{
    for(int a=1; a<nperm+1;a++){
      List ldf2=clone(ldf);
      for(int b=0;b<ldf.size();b++){
        DataFrame d=ldf2[b];
        DataFrame df=clone(d);
        for(int c=0;c<lables.size();c++){
          int n=lables[c]-1;
          SEXP vec=df[n];
          SEXP newvec=vec_sample_all(vec);
          df[n]=newvec;
        }
        ldf2[b]=df;
      }
      DataFrame tmp=ldf_merge(ldf2);
      tmp.attr("permutation") = a;
      pldf[a]=tmp;
    }
  }
  return pldf;
}

