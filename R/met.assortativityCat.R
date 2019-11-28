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

#' @title Assortativity
#' @description Calculates the binary or weighted version of vertices Newman's assortativity for categorical or continuous attributes.
#' @param M a square adjacency matrix.
#' @param attr a factor or character vector indicating individuals categorical attributes appartenance.
#' @return an integer representing the categorical assortativity index of the network.
#' @details Assortativity (like the E-I index or the Moran 'I' statistic)  allows the study of homophily (preferential interaction between nodes with similar attributes) and heterophily (the preferential interaction between nodes with different attributes). Attributes can be individual characteristics such as sex or age, or individual node metrics such as the met.degree, in which case it is referred to as assortativity by vertex met.degree.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @references Newman, M. E. (2003). Mixing patterns in networks. Physical Review E, 67(2), 026126.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @keywords internal


met.assortativityCat <- function(M, attr, df = NULL) {
  # Handling NA
  test = which(is.na(attr) == TRUE)
  if(length(test) != 0){
    attr = attr[-test]
    M = M[-test, -test]
  }
  # If argument df is null
  if (is.null(df)) {
    # Compute categorical assortativity
    tmp <- met_assor_cat(M, attr)
    # Creating mixing matrix
    colnames(tmp[[2]]) <- tmp[[3]]
    rownames(tmp[[2]]) <- tmp[[3]]
    result <- list("assortativity.cat" = tmp[[1]], "mixing.mat" = tmp[[2]])
    # Return result
    return(result)
  }
  # Else
  else {
    # Compute categorical assortativity and add it to argument data frame by creating a new column named 'assortativity.cat'.
    df$assortativity.cat <- met_assor_cat(M, attr)[[1]]
    return(df)
  }
}
