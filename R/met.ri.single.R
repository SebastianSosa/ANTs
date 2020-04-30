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
  # Compute network metric
  ri <- met.outstrength(M) / (met.outstrength(M) + met.instrength(M))
  # If argument df is null
  if (is.null(df)) {    
    attr(ri, "names") <- colnames(M)
    return(ri)
  }
  else {
    if (is.data.frame(df) == FALSE) {
      stop("Argument df must be a data frame")
    }
    # If argument dfid is not null
    if (!is.null(dfid)) {
      if (is.null(colnames(M))) {
        stop("Argument M doesn't have column names")
      }
      # Order data frame according to argument dfid
      col.id <- df.col.findId(df, dfid)
      df <- merge.met(vec = ri, names = colnames(M), df = df, dfid = col.id, met = "ri")
      return(df)
    }else{
      # Add vector of network metrics in a new column
      df$ri <- ri
      return(df)
    }
  }
}
