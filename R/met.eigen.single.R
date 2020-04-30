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
met.eigen.single <- function(M, df = NULL, dfid = NULL, sym = TRUE, binary = FALSE, out = FALSE) {
  # Organizing matrix according to arguments user declaration
  if (sym & out) {
    stop("Argument out cannot be TRUE when argument sym is TRUE.")
  }
  # Transpose matrix
  if (sym == FALSE & out == FALSE) {
    M <- t(M)
  }
  # Symetrize matrix
  if (sym) {
    M <- M + t(M)
  }
  # Binarize matrix
  if (binary) {
    M <- mat.binaryzation(M)
  }

  # Compute network metric
  result <- met_eigen(M)

  # If argument df is null
  if (is.null(df)) {
    # Colnames or argument M as names of the vector
    attr(result, "names") <- colnames(M)
    return(result)
  }
  else {
    if (is.data.frame(df) == FALSE) {
      stop("Argument df must be a data frame")
    }
    
    # If argument dfid is not null
    if (!is.null(dfid)) {
      # Order data frame according to argument dfid
      col.id <- df.col.findId(df, dfid)
      if (sym == TRUE) {
        df <- merge.met(vec = result, names = colnames(M), df = df, dfid = col.id, met = "eigen")
      }
      if (sym == FALSE & out == FALSE) {
        df <- merge.met(vec = result, names = colnames(M), df = df, dfid = col.id, met = "ineigen")
      }
      if (sym == FALSE & out == TRUE) {
        df <- merge.met(vec = result, names = colnames(M), df = df, dfid = col.id, met = "outeigen")
      }
      if (binary == TRUE) {
        df <- merge.met(vec = result, names = colnames(M), df = df, dfid = col.id, met = "eigenB")
      }
      if (binary == TRUE & sym == FALSE & out == FALSE) {
        df <- merge.met(vec = result, names = colnames(M), df = df, dfid = col.id, met = "ineigenB")
      }
      if (binary == TRUE & sym == FALSE & out == TRUE) {
        df <- merge.met(vec = result, names = colnames(M), df = df, dfid = col.id, met = "outeigenB")
      }
      return(df)
    }else{
      # Add vector of network metrics in a new column
      df$eigen <- result
      if (sym == TRUE) {
        df$eigen <- result
      }
      if (sym == FALSE & out == FALSE) {
        df$ineigen <- result
      }
      if (sym == FALSE & out == TRUE) {
        df$outeigen <- result
      }
      
      if (binary == TRUE) {
        df$eigenB <- result
      }
      if (binary == TRUE & sym == FALSE & out == FALSE) {
        df$ineigenB <- result
      }
      if (binary == TRUE & sym == FALSE & out == TRUE) {
        df$outeigenB <- result
      }
      return(df)
    }

  }
}
