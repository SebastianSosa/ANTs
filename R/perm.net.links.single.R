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

#' @title Matrix edge permutations
#' @description Permute matrix links.
#' @param M a square adjacency matrix .
#' @param sym if \emph{true} will vectorize only the lower triangle.
#' @param erase.diag if \emph{true} will not keep the diagonal of the matrix.
#' @param nperm number of permutations wanted.
#' @param progress a boolean indicating if the permutation process has to be visible.
#' @details Edge permutations can be used to create random networks based on the observed network. Such permutation method is useful when analysing patterns of interactions such as assortativity.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal

perm.net.links.single <- function(M, sym = F, erase.diag = T, nperm, progress = T) {
  if (progress) {
    if (sym) {
      if (erase.diag == T) {
        result <- lapply(seq_len(nperm), function(x, y) {
          cat("Permutation: ", x, "\r")
          y[lower.tri(y)] <- sample(y[lower.tri(y)])
          y[upper.tri(y)] <- y[lower.tri(y)]
          return(y)
        }, y = M)
        cat("\n")
        return(result)
      }
      else {
        result <- lapply(seq_len(nperm), function(x, y) {
          cat("Permutation: ", x, "\r")
          y[lower.tri(y, diag = T)] <- sample(y[lower.tri(y, diag = T)])
          y[upper.tri(y)] <- y[lower.tri(y)]
          return(y)
        }, y = M)
        cat("\n")
        return(result)
      }
    }
    else {
      if (erase.diag) {
        col <- ncol(M)
        ncell <- (col * col) - col
        result <- lapply(seq_len(nperm + 1), function(x, y, z) {
          if (x == 1) {
            return(y)
          }
          cat("Permutation: ", x - 1, "\r")
          perm <- sample(c(y[lower.tri(y)], y[upper.tri(y)]))
          y[lower.tri(y)] <- perm[1:(z / 2)]
          y[upper.tri(y)] <- perm[((z / 2) + 1):z]
          return(y)
        }, y = M, z = ncell)
        cat("\n")
        return(result)
      }
      else {
        col <- ncol(M)
        result <- lapply(seq_len(nperm + 1), function(x, y, z) {
          if (x == 1) {
            return(y)
          }
          cat("Permutation: ", x - 1, "\r")
          r <- matrix(sample(y), col, col)
          colnames(r) <- colnames(y)
          row.names(r) <- rownames(y)
          return(r)
        }, y = M, z = col)
        cat("\n")
        return(result)
      }
    }
  }
  else {
    if (sym == T) {
      if (erase.diag == T) {
        result <- lapply(seq_len(nperm + 1), function(x, y, z) {
          if (x == 1) {
            return(y)
          }
          y[lower.tri(y)] <- sample(y[lower.tri(y)])
          y[upper.tri(y)] <- y[lower.tri(y)]
          return(y)
        }, y = M)
        cat("\n")
        return(result)
      }
      else {
        result <- lapply(seq_len(nperm + 1), function(x, y, z) {
          if (x == 1) {
            return(y)
          }
          y[lower.tri(y, diag = T)] <- sample(y[lower.tri(y, diag = T)])
          y[upper.tri(y)] <- y[lower.tri(y)]
          return(y)
        }, y = M)
        cat("\n")
        return(result)
      }
    }
    else {
      if (erase.diag == T) {
        col <- ncol(M)
        ncell <- (col * col) - col
        result <- lapply(seq_len(nperm + 1), function(x, y, z) {
          if (x == 1) {
            return(y)
          }
          perm <- sample(c(y[lower.tri(y)], y[upper.tri(y)]))
          y[lower.tri(y)] <- perm[1:(z / 2)]
          y[upper.tri(y)] <- perm[((z / 2) + 1):z]
          return(y)
        }, y = M, z = ncell)
        cat("\n")
        return(result)
      }
      else {
        col <- ncol(M)
        result <- lapply(seq_len(nperm + 1), function(x, y, z) {
          if (x == 1) {
            return(y)
          }
          r <- matrix(sample(y), col, col)
          colnames(r) <- colnames(y)
          row.names(r) <- rownames(y)
          return(r)
        }, y = M, z = col)
        cat("\n")
        return(result)
      }
    }
  }
}
