# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit Software (ANTs).
#
# ANT is a free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# ANT is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

#' @title Density
#' @description Calculates network binary density.
#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @return
#' \itemize{
#' \item a double representing the density of the network if argument \emph{M} is a square matrix.
#' \item A list of doubles if argument \emph{M} is a list of matrices and if argument \emph{df} is \emph{NULL}. Each double represents the density of the corresponding matrix of the list.
#' }
#' @details Binary network density is sthe ratio of existing links of a network in relation to all potential links.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' met.density(sim.m)

met.density <- function(M, df = NULL) {
  # Checking if argument M is a square matrix 
  test <- is.matrix(M)
  if (test) {
    # Compute network metric
    result <- met_density(M)    
    if (is.null(df)) {
      names(result) <- "Density"
      return(result)
    }
    else {
      # Adding network metric in argument df
      df$density <- result
      return(df)
    }
  }
  else {
    # Check if argument M is an object returned by perm.ds.grp, perm.ds.focal or perm.net.nl or ego. ANT function----------------------
    #  ego. ANT function is being developed and is not implemented in this version of ANTs
    if (all(c(!is.null(attributes(M)$ANT), 
        attributes(M)$ANT != "Ego-network list", 
        attributes(M)$ANT != "Ego-network list whitout ego")==TRUE)) {
      
      # Check if argument M originates from ANTs multiples matrices importations
      test1 <- attributes(M)$ANT == "list of matrices obtained through data frames of interactions"
      
      if(test1){
        # Check if argument dfid is NULL
        if (is.null(df)) {
          result <- lapply(M, met_density)
          return(result)
        }
        # Check if argument df is not NULL, is not a data frame and is a list
        if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
          result <- mapply(function(x, y) {
            y$density <- met_density(x)
            return(y)
          }, x = M, y = df, SIMPLIFY = FALSE)
          return(result)
        }
      }
      else{
        # None of the permutation approches generate network density variation, thus density can be tested for permuted approaches
        stop("None of the permutation approaches available in ANT allow to make density variation.")
      }

    }
    # If argument M is a list of square matrices----------------------
    else {
      if (!test & is.list(M)) {
        # Check if argument dfid is NULL
        if (is.null(df)) {
          result <- lapply(M, met_density)
          return(result)
        }
        # Check if argument df is not NULL, is not a data frame and is a list
        if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
          result <- mapply(function(x, y) {
            y$density <- met_density(x)
            return(y)
          }, x = M, y = df, SIMPLIFY = FALSE)
          return(result)
        }
      }
    }
  }
}
