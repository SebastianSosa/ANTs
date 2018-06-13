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

#' @title Matrix to edge list
#' @description Converts a square adjacency matrix into a data frame of three columns representing an edge list. Columns are: actor, receiver and weight.

#' @param M a square adjacency matrix.
#' @param sym if \emph{true}, it extracts the lower triangle of the matrix only.
#' @param erase.diag if \emph{true}, it omits diagonals.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' mat.to.edgl(sim.m, sym = FALSE, erase.diag = TRUE)

mat.to.edgl<-function(M,sym=F,erase.diag=T){
  if (sym) {
    if(erase.diag==T){
      test=lower.tri(M)
    }
    else{
      test=lower.tri(M,diag = T)
    }
    weight=M[test]
    tmp=which(test,arr.ind = T)
    DF=cbind('from'=colnames(M)[tmp[,1]],'to'=colnames(M)[tmp[,2]])
    DF=data.frame(DF,weight)
  }
  else{
    from=rep(c(colnames(M)),dim(M)[2])
    to=rep(c(colnames(M)),each=dim(M)[1])
    weight=as.vector(M)
    DF=data.frame(from,to,weight)
    if(erase.diag==T){
      diagonals=which(DF$from==DF$to)
      DF=DF[-c(diagonals),]
    }
  }
  return(DF)
}

