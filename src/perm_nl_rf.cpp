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
List ldf_merge(List ldf);
SEXP vec_sample_all(SEXP vec);
//' @title Node lable permutation with random factors
//' @description Perorm node label permutation on list of data frames
//' @keywords internal
// [[Rcpp::export]]
List perm_nl_rf(List ldf, NumericVector lables, int nperm, bool progress) {
  // Object storing the result
  List pldf(nperm+1);
  // First data frame is se fusion of all data frames in the list
  DataFrame df= ldf_merge(ldf);
  pldf[0]=clone(df);
  
  // Creating a list of list of vector for each label declare by user
  int n_df=ldf.size();
  List lable_to_permut(n_df);
  int n=0;
  
  //Extracting first element
  DataFrame d1=Rcpp::as<Rcpp::DataFrame>(ldf[0]);

  if(progress==TRUE){
    // For each permutations declared
    for(int a=1; a<nperm+1;a++){
      Rcpp::Rcout<<"\r"<<"permutation: "<<a;

      //For each lables declared
      for(int b=0;b<lables.size();b++){
        // C++ count
        n=lables[b]-1;
        
        // Extract the column
        SEXP vec_all=d1[n];
        vec_all=vec_sample_all(vec_all);
       
        
        // For each elements of the list
        for(int c=1;c<ldf.size();c++){
          // Extract data frame
          DataFrame d=Rcpp::as<Rcpp::DataFrame>(ldf[c]);
          // Extract the column
          SEXP vec=d[n];
          // Sample it
          SEXP newvec=vec_sample_all(vec);
          // Merge it with previous labels vector in previous data frames
          vec_all=vec_merge(vec_all, newvec);
        }
        df[n]=vec_all;

      }
      df.attr("permutation") = a;
      pldf[a]=clone(df);
    }
    Rcpp::Rcout<<"\n"<<std::endl;
  }
  else{
    // For each permutations declared
    for(int a=1; a<nperm+1;a++){

      for(int b=0;b<lables.size();b++){

        // C++ count
        n=lables[b]-1;
        // Extract the column
        SEXP vec_all=d1[n];
        vec_all=vec_sample_all(vec_all);

        
        // For each elements of the list
        for(int c=1;c<ldf.size();c++){
          // Extract data frame
          DataFrame d=Rcpp::as<Rcpp::DataFrame>(ldf[c]);
          // Extract the column
          SEXP vec=d[n];
          
          // Sample it
          SEXP newvec=vec_sample_all(vec);
          
          // Merge it with previous labels vector in previous data frames
          vec_all=vec_merge(vec_all, newvec);

        }
        
        df[n]=vec_all;
        
      }
      
      df.attr("permutation") = a;
      pldf[a]=clone(df);
    }
  }
  return pldf;
}

