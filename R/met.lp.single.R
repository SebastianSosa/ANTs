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

#' @title Symetric Laplacian centrality
#' @description Calculate the symetric version of the Laplacian centrality for each verteces.

#' @param M a square adjacency matrix.
#' @param df a data frame of same length of the input matrix.
#' @param dfid an integer indicating the column of individual ids in argument \emph{df}
#' @param binary a boolean indicating if the binary version of the laplacian centrality has to be computed.

#' @details Laplacian centrality is the drop in the Laplacian energy of the graph when the vertex is removed.
#' This version uses the degrees (for the binary version ) or the strength (for the weigthed version) to calculate laplacian centrality.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references REF laplacian !!!!!!!!!!!!!!!!
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @keywords internal

met.lp.single <- function(M, df = NULL, dfid = NULL, binary = F) {
  if (binary == F) {
    if (is.null(df)) {
      result <- met.lpcW(M)
      attr(result, "names") <- colnames(M)
      return(result)
    }
    else {
      if (!is.null(dfid)) {
        if (is.null(colnames(M))) {
          stop("Argument M doesn't have column names")
        }
        col.id <- df.col.findId(df, dfid)
        df <- df[match(colnames(M), df[, col.id]), ]
      }
      if (is.data.frame(df) == F) {
        stop("Argument df must be a data frame")
      }
      df$lp <- met.lpcW(M)
      return(df)
    }
  }
  if (binary == T) {
    if (is.null(df)) {
      result <- met.lpcB(M)
      attr(result, "names") <- colnames(M)
      return(result)
    }
    else {
      if (!is.null(dfid)) {
        col.id <- df.col.findId(df, dfid)
        df <- df[match(colnames(M), df[, col.id]), ]
      }
      else {
        cat("Argument dfid hasn't been declared. M and df are considered to be ordered exactly in the same way.", "\n")
      }
      if (is.data.frame(df) == F) {
        stop("Argument df must be a data frame")
      }
      result <- met.lpcB(M)
      df$lpB <- result
      return(df)
    }
  }
}
