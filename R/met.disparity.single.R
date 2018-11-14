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

#' @title Disparity
#' @description Calculate for all the vertices the node metric call met.disparity.

#' @param M a square adjacency matrix.
#' @param df a data frame of same length of the input matrix.
#' @param dfid an integer indicating the column of individual ids in argument \emph{df}
#' @param directed if \emph{true}  then calculate the met.disparity directed version \emph{i.e.} in-met.disparity, out-met.disparity, met.disparity.
#' @return
#' \itemize{
#' \item For the undirected version: Return an integer vector of each vertices met.disparity.
#' \item For the undirected version: Return a data frame of three columns: met.disparity,inDisparity,outDisparity.
#' }

#' @details Disparity measures the weight variation of a node. This metric compares a node's strength with its degree. It provides information on the type of edges found in a node \emph{i}, \emph{i.e.} few strong edges or many weak edges
#' met.disparity of a vertice \emph{i} is the summe of the squares of the division between the weigthed edge between node \emph{i} and \emph{j} and the met.strength of node \emph{i}
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @keywords internal

met.disparity.single <- function(M, df = NULL, dfid = NULL) {
  s <- met.strength(M) / 2
  m.strength <- matrix(rep(s), ncol = ncol(M), nrow = nrow(M), byrow = T)
  tmp <- rowSums((M / m.strength)^2)

  if (is.null(df)) {
    return(tmp)
  }
  else {
    if (is.null(dfid)) {
      df$disparity <- tmp
      return(df)
    }
    else {
      col.id <- df.col.findId(df, dfid)
      df <- df[match(colnames(M), df[, col.id]), ]
      df$disparity <- tmp
      return(df)
    }
  }
}