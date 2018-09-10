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

#' @title Outstrength
#' @description Calculate for all the vertices the node metric call outstrentgh.

#' @param M a square adjacency matrix.
##' @param df a data frame of same length of the input matrix.
#' @param dfid an integer indicating the column of individual ids in argument \emph{df}
#' @return Integer vector of each vertices outstrentgh.

#' @details  met.instrength of vertice \emph{i} is the met.strength of all edges emit by node \emph{i}.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references Barrat, A., Barthelemy, M., Pastor-Satorras, R., & Vespignani, A. (2004). The architecture of complex weighted networks. Proceedings of the National Academy of Sciences of the United States of America, 101(11), 3747-3752.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.

#' @keywords internal


met.outstrength.single <- function(M, df = NULL, dfid = NULL) {
  if (is.null(df)) {
    outstrength <- mat_rows_sums(M)
    attr(outstrength, "names") <- colnames(M)
    return(outstrength)
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
    outstrength <- mat_rows_sums(M)
    df$outstrength <- outstrength
    return(df)
  }
}
