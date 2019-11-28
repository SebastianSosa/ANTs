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
//' @title Association indexes for one individual
//' @description Individual associations indexes.
//' @param Mfbi a group by individual matrix.
//' @param id an integer indicating the column (c++ refering) of the individual to which compute the association index.
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
arma::rowvec assoc_mat_one_id (arma::mat Mgbi,int id,std::string method){
  if((method!="sri") & (method!="hwi") & (method!="sqri")){
    Rcpp::stop("Argument method doesn't match. Only 'simple ratio', 'half-weight' or 'square root' methods are available.");
  }
  int groups = Mgbi.n_rows;
  int Ind = Mgbi.n_cols;
  arma::mat presence(groups,Ind); presence.zeros();
  arma::mat abscence(groups,Ind); abscence.zeros();
  for (int group = 0; group < groups; group++){
  if(Mgbi(group,id)==1){presence.row(group)=Mgbi.row(group);}
  if(Mgbi(group,id)==0){abscence.row(group)=Mgbi.row(group);}
  }
  arma::rowvec x = sum(presence,0);
  int Apres = sum(presence.col(id));
  arma::rowvec Apresence(Ind); Apresence.fill(Apres);
  arma::rowvec xa = Apresence-x;
  arma::rowvec xb = sum(abscence,0);
  
  arma::rowvec association(1,Ind);
  if(method== "sri"){
    association=x.row(0)/(x.row(0)+xa.row(0)+xb.row(0));;
  }
  if(method== "hwi"){
    association=x.row(0)/(x.row(0)+((xa.row(0)+xb.row(0))/2));
  }
  if(method== "sqri"){
    arma::mat denom=sqrt(x.row(0)+xa.row(0)+xb.row(0));
    association=x.row(0)/(denom*denom);
  }
  return association;
 }

