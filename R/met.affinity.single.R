# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit Software (ANTs).
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

#' @title Affinity
#' @description Calculate for all the vertices the node metric call Affinity

#' @param M a square adjacency matrix.
#' @param df a data frame of same length of the input matrix.
#' @param binary a boolean, if \emph{true}, it calculates the binary version of the affinity.
#' @return Integer vector of each vertices met.affinity.

#' @details  Affinity is a second order metric that try to evaluate to which egos, node \emph{in} is connected. The binary version  is simply the average degree of alters of node i. The weighted version is ratio between the met.reach metric and the met.strength of node i.A high met.affinity reveals that nodes tend to be connected to alters of high degrees or strengths. Thus, both metrics provide information on node assortativity by vertex met.degree, i.e. connections between nodes with similar degrees or strengths.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @keywords internal

met.affinity.single <- function(M, df = NULL, dfid = NULL, binary = FALSE) {
  # Compute network metric
  if(binary){
    affinity=met_sum_egos_strength(M)
  }
  else{
    reach <- met.reach.single(M, return.strength = TRUE)
    affinity <- reach[[1]] / reach[[2]]
  }

  # If argument df is null
  if (is.null(df)) {
    # Colnames or argument M as names of the vector
    attr(affinity, "names") <- colnames(M)
    return(affinity)
  }

  else {
    # If argument dfid is not null
    if (is.data.frame(df) == FALSE) {
      stop("Argument df must be a data frame")
    }
    if (!is.null(dfid)) {
      if (is.null(colnames(M))) {
        stop("Argument M doesn't have column names")
      }
      # Order data frame according to argument dfid
      col.id <- df.col.findId(df, dfid)
      if (binary) {
        df <- merge.met(vec = affinity, names = colnames(M), df = df, dfid = col.id, met = "affinityB")
      }else {
        df <- merge.met(vec = affinity, names = colnames(M), df = df, dfid = col.id, met = "affinity")      
      }
      return(df)
    }else{
      # Add vector of network metrics in a new column
      if (binary) {
        df$affinityB <- affinity
      }
      else {
        df$affinity <- affinity
      }
      return(df)
    }
  }
}
