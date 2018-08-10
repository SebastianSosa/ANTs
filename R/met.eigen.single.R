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

#' @title Eigenvector Centrality
#' @description Calculate for all the vertices the node metric call met.evcent centrality.

#' @param M a square adjacency matrix.
#' @param df a data frame of same length of the input matrix.
#' @param dfid an integer indicating the column of individual ids in argument \emph{df}
#' @return Integer vector of each met.evcent centrality.

#' @details met.evcent centrality is the first non-negative met.evcent value obtained through the linear transformation of an adjacency matrix. This centrality measure quantifies not only a node connectedness, but also the connections of the nodes to whom it is connected. Thus, a node can have a high met.evcent value by having a high met.degree or met.strength, or by being connected to nodes that have high degrees or strengths.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez

#' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @keywords internal
#'
met.eigen.single <- function(M, df = NULL, dfid = NULL, sym = T, binary = F, out = F) {
  if (sym & out) {
    stop("Argument out cannot be TRUE when argument sym is TRUE.")
  }
  if (sym == F & out == F) {
    M <- t(M)
  }
  if (sym) {
    M <- M + t(M)
  }
  if (binary) {
    M <- mat.binaryzation(M)
  }

  if (is.null(df)) {
    result <- met_eigen(M)
    attr(result, "names") <- colnames(M)
    return(result)
  }
  else {
    if (!is.null(dfid)) {
      col.id <- df.col.findId(df, dfid)
      df[match(colnames(M), df[, col.id]), ]
    }
    if (is.data.frame(df) == F) {
      stop("Argument df must be a data frame")
    }
    result <- met_eigen(M)
    df$eigen <- result
    if (sym == T) {
      df$eigen <- result
    }
    if (sym == F & out == F) {
      df$ineigen <- result
    }
    if (sym == F & out == T) {
      df$outeigen <- result
    }

    if (binary == T) {
      df$eigenB <- result
    }
    if (binary == T & sym == F & out == F) {
      df$ineigenB <- result
    }
    if (binary == T & sym == F & out == T) {
      df$outeigenB <- result
    }
    return(df)
  }
}
