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
Rcpp::NumericVector stat_chol2inv(Rcpp::NumericMatrix M);
// [[Rcpp::export]]
NumericVector stat_t_value(List lm, NumericMatrix x4) {
  NumericVector coef= lm["coefficients"];
  NumericVector resid= lm["residuals"];
  int rank= lm["rank"];
  int n=resid.size();
  NumericVector diag_chol2inv=stat_chol2inv(x4);
  
  NumericVector tmp=abs(resid);
  double tmp2=sum(tmp*tmp);

  return (coef/sqrt(diag_chol2inv * (tmp2/(n-rank))));
}

