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

#' @title Matrix links permutations
#' @description Permutes matrix links.
#' @param M a square adjacency matrix or a list of square adjacency matrices.
#' @param sym if \emph{true} it vectorizes the lower triangle only .
#' @param erase.diag if \emph{true} it erases the diagonal of the matrix.
#' @param nperm number of permutations wanted.
#' @param progress a boolean indicating if the permutation process must be visible.
#' @details Edge permutations can be used to create random networks based on the observed network. Such permutation method is useful when analysing patterns of interaction such as assortativity.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' t=perm.net.lk(sim.m, sym = FALSE, erase.diag = TRUE, nperm=10, progress=TRUE)

perm.net.lk <- function(M, sym = F, erase.diag = T, nperm, progress = T) {
  if (is.list(M)) {
    result <- lapply(M, perm.net.links.single, sym = sym, erase.diag = erase.diag, nperm = nperm, progress = progress)
    attr(result, "ANT") <- "ANT data stream group sampling multiple matrices"
  }
  else {
    result <- perm.net.links.single(M, sym = sym, erase.diag = erase.diag, nperm = nperm, progress = progress)
    attr(result, "ANT") <- "ANT link permutations single matrix"
  }

  return(result)
}
