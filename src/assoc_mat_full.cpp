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

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
//' @title Association indexes
//' @description Individual associations indexes.
//' @param Mfbi a group by individual matrix.
//' @param method a string indicating the type of association matrix:
//' \itemize{
//' \item 'sri' for Simple ratio index: \eqn{x/x+yAB+yA+yB}
//' \item 'hw' for Half-weight index: \eqn{x/x+yAB+1/2(yA+yB)}
//' \item 'sr' for Square root index:\eqn{x/sqr((x+yAB+yA)(x+yAB+yB))}
//' }
//' @return A square matrix of association according to association index choose:
//' @details Association indexes allow to handle  heterogeneity of time of observation. 
//' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
//' @author Sebastian Sosa, Ivan Puga-Gonzales.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List assoc_mat_full (arma::mat Mgbi, std::string method){
  Rcpp::List result(3);
  int groups = Mgbi.n_rows;
  int Ind = Mgbi.n_cols;
  arma::mat mx(Ind,Ind); mx.zeros();
  arma::mat mxa(Ind,Ind); mxa.zeros();
  arma::mat mxb(Ind,Ind); mxb.zeros();
  for (int A = 0; A < Ind; A++) {
    arma::mat presence(groups,Ind); presence.zeros();
    arma::mat abscence(groups,Ind); abscence.zeros();
    for (int group = 0; group < groups; group++){
      if(Mgbi(group,A)==1){presence.row(group)=Mgbi.row(group);}
      if(Mgbi(group,A)==0){abscence.row(group)=Mgbi.row(group);}
    }
    arma::rowvec x = sum(presence,0);
    int Apres = sum(presence.col(A));
    arma::rowvec Apresence(Ind); Apresence.fill(Apres);
    arma::rowvec xa = Apresence-x;
    arma::rowvec xb = sum(abscence,0);
    mx.row(A)=x;
    mxa.row(A)=xa;
    mxb.row(A)=xb;
  }
  
  if((method!="sri") & (method!="hwi") & (method!="sqri")){
    Rcpp::stop("Argument method doesn't match. Only 'simple ratio', 'half-weight' or 'square root' methods are available.");
  }
  arma::mat association(Ind,Ind);
  arma::mat denominator(Ind,Ind);
  if(method== "sri"){
    association=mx/(mx+mxa+mxb);
    association.diag().zeros();
    
    denominator=(mx+mxa+mxb);
    
    mx.diag().zeros();
    denominator.diag().zeros();

    result[0]=association;
    result[1]=mx;
    result[2]=denominator;
  }
  if(method== "hwi"){
    association=mx/(mx+((mxa+mxb)/2));
    association.diag().zeros();
    
    denominator=(mx+((mxa+mxb)/2));
    
    mx.diag().zeros();
    denominator.diag().zeros();
    
    result[0]=association;
    result[1]=mx;
    result[2]=denominator;
  }
  if(method== "sqri"){
    arma::mat denom=sqrt(mx+mxa+mxb);
    association=mx/(denom*denom);
    association.diag().zeros();
    
    denominator=denom*denom,
      
    mx.diag().zeros();
    denominator.diag().zeros();
    
    result[0]=association;
    result[1]=mx;
    result[2]=denominator;
  }

  return result;
}
