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
NumericVector vec_id_sup0( NumericVector x);

// [[Rcpp::export]]
NumericVector met_cc(NumericMatrix& M, std::string method) {
  int S=M.ncol();
  
  NumericVector degree(S);
  NumericVector strtength(S);
  for(int a=0; a<S; a++){
    NumericVector r=M(a,_);
    NumericVector c=M(_,a);
    NumericVector row=vec_id_sup0(r);
    NumericVector col=vec_id_sup0(c);
    degree[a]=row.size()+col.size();
    strtength[a]=sum(r)+sum(c);
  }
  
  int n_try=0;
  int ego_alter1;
  int ego_alter2;
  int alter_alter;
  NumericVector w;
  
  NumericVector cc(S);
  for(int a=0; a<S; a++){
    NumericVector c2=M(_,a);
    NumericVector ego_alters=vec_id_sup0(c2)-1; //alters of ego
    std::cout<<ego_alters<<std::endl;
    
    int tot_w(ego_alters.size());
    for(int b=0; b<ego_alters.size(); b++){
      NumericVector c3=M(_,b);
      NumericVector alter_alters=vec_id_sup0(c3)-1; //alter alters
      IntegerVector share_alters=match(alter_alters,ego_alters); // common alter share
      std::cout<<share_alters<<std::endl;
      
        NumericVector w(share_alters.size());
        for(int c=0; c<share_alters.size(); c++){
          n_try=n_try+share_alters.size();
          
          ego_alter1=M(a,ego_alters[b])+M(ego_alters[b],a);
          std::cout<<ego_alter1<<std::endl;
          
          ego_alter2=M(a,share_alters[c])+M(share_alters[c],a);
          std::cout<<ego_alter2<<std::endl;
          
          alter_alter=M(ego_alters[b],share_alters[c])+M(share_alters[c],ego_alters[b]);
          std::cout<<alter_alter<<std::endl;
          
          if(method=="sum"){
            w[c]=ego_alter1+ego_alter2+alter_alter;
          }
          
        }
        tot_w=sum(w);
        std::cout<<w<<std::endl;
        std::cout<<tot_w<<std::endl;
      
    }
    cc[a]=1/(strtength[a]*(degree[a]-1))*tot_w;
  }
  return cc;
}