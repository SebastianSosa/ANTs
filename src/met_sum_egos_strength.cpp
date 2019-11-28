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
NumericVector met_sum_egos_strength(NumericMatrix M) {
  
  int ncol=M.cols();
  NumericVector affinity(ncol);

  for(int ego=0;ego<ncol;ego++){
    NumericVector row_ego=M(ego,_);
    affinity(ego)=std::accumulate(row_ego.begin(), row_ego.end(), 0.0);
    
  }
 return affinity;
}


