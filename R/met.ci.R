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

#' @title Centralisation index
#' @description Computes network Centralisation index
#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param binary a boolean, if \emph{TRUE}, it calculates the binary version of the affinity.
#' @param sym if \emph{TRUE}, then it symmetrizes the matrix. Otherwise, it calculates geodesic distances and diameter according to the directionality of the links.
#' @details Centralisation index of a network is based on eigenvector centrality.
#' @return
#' #' \itemize{
#' \item a double representing the centralisation index of the network if argument \emph{M} is a square matrix.
#' \item A list of doubles if argument \emph{M} is a list of matrices and if argument \emph{df} is \emph{NULL}. Each double represents the centralisation index of the corresponding matrix of the list.
#' \item A list of arguments \emph{df} with a new column of network centralisation index if argument\emph{df} is not \emph{NULL} and if argument \emph{M} is a list of matrices. The name of the column is adapted according to arguments values \emph{binary} and \emph{sym}.
#' \item A list of arguments \emph{df} with a new column of network centralisation index if argument \emph{df} is not \emph{NULL}, if argument \emph{M} is an output from ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk} for multiple matrices permutations, and if argument \emph{df} is a list of data frames of same length as argument \emph{M}.
#' }
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @references Pasquaretta, C., Levé, M., Claidiere, N., Van De Waal, E., Whiten, A., MacIntosh, A. J., ... & Crofoot, M. C. (2014). Social networks in primates: smart and tolerant species have more efficient networks. Scientific reports, 4, 7600.
#' @examples
#' met.ci(sim.m)

met.ci <- function(M, df = NULL, binary = F, sym = T) {
  test <- is.matrix(M)
  if (test) {
    result <- met.ci.single(M)
    if (is.null(df)) {
      return(result)
    }
    else {
      df$ci <- result
      return(df)
    }
  }
  else {
    if (!is.null(attributes(M)$ANT)) {
      test1 <- attributes(M)$ANT == "ANT data stream focal sampling single matrix"
      test2 <- attributes(M)$ANT == "ANT data stream group sampling single matrix"
      test3 <- attributes(M)$ANT == "ANT link permutations single matrix"

      test4 <- attributes(M)$ANT == "ANT data stream focal sampling multiple matrices"
      test5 <- attributes(M)$ANT == "ANT data stream group sampling multiple matrices"
      test6 <- attributes(M)$ANT == "ANT link permutations multiple matrices"

      if (any(test1, test2, test3)) {
        if (is.null(df)) {
          result <- lapply(M, function(x) {
            r <- met.ci.single(x)
            attr(r, "permutation") <- attributes(x)$permutation
            return(r)
          })
        }
        else {
          if (!is.data.frame(df)) {
            stop("Argument df must be a data frame when argument M is an outcome of perm.ds.grp ant function", "\r")
          }
          result <- lapply(M, function(x, df) {
            df$ci <- met.ci.single(x)
            attr(df, "permutation") <- attributes(x)$permutation
            return(df)
          }, df = df)
        }

        if (test1) {
          attr(result, "focal") <- attributes(M)$focal
          attr(result, "ctrl") <- attributes(M)$ctrl
          attr(result, "alters") <- attributes(M)$alters
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }

        if (test2) {
          attr(result, "scan") <- attributes(M)$scan
          attr(result, "ctrlf") <- attributes(M)$ctrlf
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }

        if (test3) {
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }
      }

      if (any(test4, test5, test6)) {
        if (is.null(df)) {
          result <- lapply(M, function(x) {
            r1 <- lapply(x, function(y) {
              r2 <- met.ci.single(y)
              attr(r2, "permutation") <- attributes(y)$permutation
              return(r2)
            })
            return(r1)
          })
          attr(result, "scan") <- attributes(M)$scan
          attr(result, "ctrlf") <- attributes(M)$ctrlf
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }
        else {
          if (!is.null(df) & is.data.frame(df)) {
            stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
          }
          if (length(M) == nrow(df)) {
            tmp <- lapply(M, function(x) {
              r1 <- lapply(x, function(y) {
                r2 <- met.ci.single(y)
              })
            })

            tmp <- do.call(Map, c(c, tmp))

            result <- lapply(seq_along(tmp), function(x, tmp, df) {
              df[[x]]$ci <- tmp[[x]]
              return(df[[x]])
            }, tmp = tmp, df = df)
          }
          else {
            # data fame manipulation
            ldf <- do.call("rbind", df)

            tmp <- lapply(M, function(x) {
              r1 <- lapply(x, function(y) {
                r2 <- met.ci.single(y)
              })
            })

            tmp <- do.call(Map, c(c, tmp))

            result <- lapply(seq_along(tmp), function(tmp, ldf, i) {
              ldf$ci <- tmp[[i]]
              attr(ldf, "permutation") <- i
              return(ldf)
            }, tmp = tmp, ldf = ldf)
          }
        }

        if (test4) {
          attr(result, "focal") <- attributes(M)$focal
          attr(result, "ctrl") <- attributes(M)$ctrl
          attr(result, "alters") <- attributes(M)$alters
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }

        if (test5) {
          attr(result, "scan") <- attributes(M)$scan
          attr(result, "ctrlf") <- attributes(M)$ctrlf
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }

        if (test6) {
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }
      }
    }
    else {
      if (!test & is.list(M)) {
        if (is.null(df)) {
          result <- lapply(M, met.ci.single)
          return(result)
        }

        if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
          result <- mapply(function(x, y) {
            y$ci <- met.ci.single(x)
            return(y)
          }, x = M, y = df, SIMPLIFY = F)
          return(result)
        }
      }
    }
  }
}
