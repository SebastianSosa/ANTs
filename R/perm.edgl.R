# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
#
# ANT is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# ANT is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

#' @title Edge list link permutations on an edgelist
#' @description Permutes the links of an edge list represented by a data frame.
#' @param edgelist an edgelist
#' @param perm number of permutations to perform.
#' @return a list of edge lists with permuted links. The first element of the list is the original edge list.
#' @details edge list links permutations can be used to create random networks based on the observed network. Such permutation method is useful when analysing patterns of interactions such as assortativity.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal

perm.edgl<-function(edgelist,perm){
  MP<-list()
  for (i in 1:perm)
  {
  		Perm<-sample(edgelist$weight)
  		dfp<-edgelist
    	dfp$weight<-Perm
    	MP[[i]]<-edgl_to_matrix(dfp)
  }
  return(MP)
}