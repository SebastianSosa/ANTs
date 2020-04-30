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

#' @title Reach centrality
#' @description Calculate for all the vertices the node metric call reach.

#' @param M a square adjacency matrix.
#' @param df a data frame of same length of the input matrix.
#' @param dfid an integer indicating the column of individual ids in argument \emph{df}.
#' @param return.strength a boolean that indicate if strength have to be return. Possiblility only available if if argument \emph{df} and \emph{dfid} are both \emph{NULL} Default value is \emph{FALSE}.
#' @return  Integer vector of each vertices R-Index values.

#' @details  reach is the sum of the product of all the ego and alter's strengths and the alters' degrees
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal


met.reach.single <- function(M, df = NULL, dfid = NULL, return.strength = FALSE) {
  # Compute node instrength
  if (isSymmetric(M)) {
    s <- met.instrength(M)
  }
  # Compute node strength
  else {
    s <- met.strength(M)
  }

  # If argument df is null
  if (is.null(df)) {
    # Compute network metric
    m.strength <- matrix(rep(s), ncol = ncol(M), nrow = nrow(M), byrow = TRUE)
    result <- rowSums(m.strength * M)
    attr(result, "names") <- colnames(M)
    if (return.strength) {
      return(list("reach" = result, "strength" = s))
    }
    else {
      return(result)
    }
  }
  else {
    if (is.data.frame(df) == FALSE) {
      stop("Argument df must be a data frame")
    }
    
    # Compute network metric
    m.strength <- matrix(rep(s), ncol = ncol(M), nrow = nrow(M), byrow = TRUE)
    result <- rowSums(m.strength * M)
    
    # If argument dfid is not null
    if (!is.null(dfid)) {
      if (is.null(colnames(M))) {
        stop("Argument M doesn't have column names")
      }
      # Order data frame according to argument dfid
      col.id <- df.col.findId(df, dfid)
      df <- merge.met(vec = result, names = colnames(M), df = df, dfid = col.id, met = "reach")
      return(df)
    }else{
      df$reach <- result
      return(df)
    }
  }
}
