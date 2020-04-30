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

#' @title Link perm keeping structure on a single matrix
#' @description Permute row and/or columns of a matrix in order to keep links structure constant during perm.

#' @param M a square adjacency matrix or a list of square adjacency matrix.
#' @param perm number of perm wanted
#' @return
#' \itemize{
#' \item A list of prmuted matrices, if M is a single square adjacency matrix.
#' \item A list of list if M is a list of square adjacency matrix.
#' }
#' @details  Permutation of link and keeping link structure allow to study?????????
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal

perm.met.degree.single <- function(M, perm) {
  # list to hold the permuted matrices	
  MPerm <- list()
  MPerm[[1]] = M
  # Perform link permutations
  for (a in 2:(perm+1)) {
    pp <- sample(1:dim(M)[[2]])
    MPerm[[a]] <- M[pp, pp]
  }
  return(MPerm)
}
