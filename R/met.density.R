# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
#
# ANT is a free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# ANT is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

#' @title Density
#' @description Calculates network binary density.
#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @return
#' \itemize{
#' \item a double representing the density of the network if argument \emph{M} is a square matrix.
#' \item A list of doubles if argument \emph{M} is a list of matrices and if argument \emph{df} is \emph{NULL}. Each double represents the density of the corresponding matrix of the list.
#' }
#' @details Binary network density is sthe ratio of existing links of a network in relation to all potential links.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' met.density(sim.m)

met.density<-function(M,df=NULL){
  test=is.matrix(M)
  if(test){
    result=met_density(M)
    if(is.null(df)){
      return(result)
    }
    else{
      df$density=result
      return(df)
    }
    
  }
  else{
    if(!is.null(attributes(M)$ANT)){
      stop("None of the permutation approaches available in ANT allow to make density variation.")
    }
    else{
      if(!test & is.list(M)){
        if(is.null(df)){
          result=lapply(M,met_density)
          return(result)
        }
        
        if(!is.null(df) & !is.data.frame(df) & is.list(df)){
          result=mapply(function(x,y){
            y$density=met_density(x)
            return(y)
          },x=M,y=df,SIMPLIFY = F)
          return(result)
        }
      }
    }
  }
}
