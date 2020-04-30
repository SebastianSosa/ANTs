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

#' @title Betweenness centrality
#' @description Compute node betweenness
#' @keywords internal

met.betweenness.single <- function(m, binary = FALSE, shortest.weight = FALSE, normalization = TRUE, sym = TRUE, out = TRUE, df = NULL, dfid = NULL) {
  # Organizing matrix according to arguments user declaration
  name <- colnames(m)
  
  if (sym) {
    m <- m + t(m)
  }
  
  if (binary) {
    m <- mat_filter(m, 1, 1)
    colnames(m) <- name
    rownames(m) <- name
  }

  if (shortest.weight == FALSE) {
    # opshal method
    if (normalization) {
      number.of.links = sum(m > 0)
      avg_strength <- sum(m) / number.of.links
      m <- m / avg_strength
    }
    
    m <- 1 / m
    m[is.infinite(m)] <- 0
  }

  if (out == FALSE) {
      m <- t(m)
  }
  
  # Compute network metric
  result <- metric_node_betweeness(m)
  result
  # If argument df is null
  if (is.null(df)) {
    # Colnames or argument M as names of the vector
    attr(result, "names") <- colnames(m)
    result
    return(result)
  }
  else {
    if (is.data.frame(df) == FALSE) {
      stop("Argument df must be a data frame")
    }
    # If argument dfid is not null
    if (!is.null(dfid)) {
      if (is.null(colnames(m))) {
        stop("Argument m doesn't have column names")
      }
      # Order data frame according to argument dfid
      col.id <- df.col.findId(df, dfid)
      df <- merge.met(vec = result, names = colnames(m), df = df, dfid = col.id, met = "betweenness")
      return(df)
    }else{
      # Add vector of network metrics in a new column
      df[, ncol(df) + 1] <- result
      return(df)
    }
  }
}
