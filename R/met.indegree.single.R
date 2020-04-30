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

#' @title Indegree
#' @description Calculate for all the vertices the node metric call met.indegree.

#' @param M a square adjacency matrix.
#' @param df a data frame of same length of the input matrix.
#' @param dfid an integer indicating the column of individual ids in argument \emph{df}
#' @return Integer vector of each vertices met.indegree.

#' @details  met.indegree of vertice \emph{i} is the number of all edges receive by node \emph{i}.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references Barrat, A., Barthelemy, M., Pastor-Satorras, R., & Vespignani, A. (2004). The architecture of complex weighted networks. Proceedings of the National Academy of Sciences of the United States of America, 101(11), 3747-3752.
#' @keywords internal

met.indegree.single <- function(M, df = NULL, dfid = NULL) {
  # Compute network metric
  BID <- mat_col_sumsBinary(M)

  # If argument df is null
  if (is.null(df)) {
    # Colnames or argument M as names of the vector
    attr(BID, "names") <- colnames(M)
    return(BID)
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
      df <- merge.met(vec = BID, names = colnames(M), df = df, dfid = col.id, met = "indegree")
      return(df)
    }else{
      # Add vector of network metrics in a new column
      BID <- mat_col_sumsBinary(M)
      df$indegree <- BID
      return(df)
    }
  }
}
