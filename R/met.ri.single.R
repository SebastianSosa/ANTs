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

#' @title R-Index
#' @description Calculate for all the vertices the node metric call R-Index.

#' @param M a square adjacency matrix.
#' @param df a data frame of same length of the input matrix.
#' @param dfid an integer indicating the column of individual ids in argument \emph{df}
#' @return Integer vector of each vertices R-Index values. If the adjacency matrix is binary it will return the binari R-index. If the adjacency matrix is weigthed it will return the binari R-index.

#' @details  R-Index of vertice \emph{i} is the outstrength of node \emph{i} divided by the met.strength  of node \emph{i}. Such node metric atempt to measur if an individual have more tendency to receive or emit edges.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal

met.ri.single <- function(M, df = NULL, dfid = NULL) {
  if (is.null(df)) {
    ri <- met.outstrength(M) / (met.outstrength(M) + met.instrength(M))
    attr(ri, "names") <- colnames(M)
    return(ri)
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
    ri <- met.outstrength(M) / (met.outstrength(M) + met.instrength(M))
    df$ri <- ri
    return(df)
  }
}