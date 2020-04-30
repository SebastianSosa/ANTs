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

met.lp.single <- function(M, df = NULL, dfid = NULL, binary = FALSE) {
  # Compute network metric
  if(binary) {
    result <- met.lpcB(M)
  }
  else {    
    result <- met.lpcW(M)
  }

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
      if (is.null(colnames(M))) {
        stop("Argument M doesn't have column names")
      }
      # Order data frame according to argument dfid
      col.id <- df.col.findId(df, dfid)
      if(binary){
        df <- merge.met(vec = result, names = colnames(M), df = df, dfid = col.id, met = "lpB")
      }
      else{
        df <- merge.met(vec = result, names = colnames(M), df = df, dfid = col.id, met = "lp")
      }      
      return(df)
    }else{
      # Add vector of network metrics in a new colum
      if(binary){
        df$lpB <- result
      }
      else{
        df$lp <- met.lpcW(M)
      }      
      return(df)
    }
  }
}
