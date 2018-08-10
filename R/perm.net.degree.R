# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
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

#' @title Link permutation keeping the structure
#' @description Permutes the rows and/or the columns of a matrix to maintain the link structure during the permutation process.
#' @param M a square adjacency matrix or a list of square adjacency matrices.
#' @param nperm number of permutations wanted.
#' @return
#' \itemize{
#' \item A list of permuted matrices, if M is a single square adjacency matrix.
#' \item A list of a list of permuted matrices if M is a list of square adjacency matrices.
#' }
#' @details Link permutation maintaining the link structure, is usually used for matrix correlation or regression.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @examples
#' t=perm.net.degree(sim.m,nperm=10)

perm.net.degree <- function(M, nperm) {
  if (is.list(M) == FALSE & is.matrix(M) == FALSE) {
    stop("Argument M is incorrect, a list of matrices is needed")
  }
  if (is.list(M) == FALSE) {
    if (is.matrix(M) == FALSE) {
      stop("Argument M is incorrect, a matrix is needed")
    }
    P.M <- perm.met.degree.single(M, nperm)
  }
  else {
    if (sum(unlist(lapply(M, is.matrix))) != length(M)) {
      stop("Argument M is incorrect, one of the elements in the list is not a matrix")
    }

    P.M <- lapply(M, perm.met.degree.single, nperm)
  }
  attr(P.M, "comment") <- "ANT degree permutation"
  return(P.M)
}
