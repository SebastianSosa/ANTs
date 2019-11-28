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

#' @title Global efficiency
#' @description Calculates the global efficiency of a network.

#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param df a data frame of same length as the input matrix or a list of data frames if argument \emph{M} is a list of matrices or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param weighted if \emph{false}, it binarizes the square adjacency matrix M. Geodesic distances and diameter are based only on the presence or absence of edges.
#' @param shortest.weight if \emph{false},  and weighted is TRUE, it considers the highest weights as the shortest path. The value returned is the average of inverse of the all shortest distances (1/dij). Thus a higher value means a higher efficiency. Note that in constrast to unweighted graphs where global efficiency assumes values from 0 to 1, in weighted graphs the values depend on the weights associated to the links. It is therefore very useful to compare the global efficiency of a given weighted network with the global efficiency of a randomized version of the network or to a network of same size but different distribution of weights among the links. By itself this value is meaningless.  
#' @param normalization normalizes the weights of the links i.e. divides them by the average strength of the network.
#' @param directed if \emph{false}, it symmetrizes the matrix. Otherwise, it calculates geodesic distances and diameter according to the directionality of the links.
#' @param out if \emph{true}, it considers outgoing ties.
#' @return
#' \itemize{
#' \item a double representing the global efficiency of the network if argument \emph{M} is a square matrix.
#' \item A list of doubles if argument \emph{M} is a list of matrices and if argument \emph{df} is \emph{NULL}. Each double represents the global efficiency of the corresponding matrix of the list.
#' \item A list of arguments \emph{df} with a new column of network global efficiency if argument\emph{df} is not \emph{NULL} and if argument \emph{M} is a list of matrices. The name of the column is adapted according to arguments values \emph{.weighted}, \emph{shortest.weight}, \emph{normalization}, \emph{directed} and \emph{out}.
#' \item A list of arguments \emph{df} with a new column of network global efficiency if argument \emph{df} is not \emph{NULL}, if argument \emph{M} is an output from ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk} for multiple matrices permutations, and if argument \emph{df} is a list of data frames of same length as argument \emph{M}.
#' }
#' @details The global efficiency provides a measure of how efficiently information is exchanged in a network. It can be calculated in binary or weighted networks, and for undirected or directed networks. Depending on the problematic, care is needed for the choice of calculation method (binary or weighted, directed or undirected, and using the lowest or the highest met.strength as shortest path).
#' @author Sebastian Sosa, Ivan Puga-Gonzalez

#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' met.ge(sim.m)

met.ge <- function(M, df = NULL, weighted = TRUE, shortest.weight = FALSE, normalization = TRUE, directed = TRUE, out = TRUE) {
  test <- is.matrix(M)
  if (test) {
    result <- met.ge.single(M, weighted, shortest.weight, normalization, directed, out)
    if (is.null(df)) {
      names(result) <- "Global efficiency"
      return(result)
    }
    else {
      df$ge <- result
      return(df)
    }
  }

  else {
    tmp <- "tmp"
    if (weighted == FALSE) {
      if (normalization) {
        if (shortest.weight) {
          if (directed == FALSE) {
            attr(tmp, "name") <- "norm.short.geB"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "norm.short.outgeB"
            }
            else {
              attr(tmp, "name") <- "norm.short.ingeB"
            }
          }
        }
        else {
          if (directed == FALSE) {
            attr(tmp, "name") <- "norm.geB"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "norm.outgeB"
            }
            else {
              attr(tmp, "name") <- "norm.ingeB"
            }
          }
        }
      }
      else {
        if (shortest.weight) {
          if (directed == FALSE) {
            attr(tmp, "name") <- "short.geB"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "short.outgeB"
            }
            else {
              attr(tmp, "name") <- "short.ingeB"
            }
          }
        }
        else {
          if (directed == FALSE) {
            attr(tmp, "name") <- "geB"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "outgeB"
            }
            else {
              attr(tmp, "name") <- "ingeB"
            }
          }
        }
      }
    }
    else {
      if (normalization) {
        if (shortest.weight) {
          if (directed == FALSE) {
            attr(tmp, "name") <- "norm.short.ge"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "norm.short.outge"
            }
            else {
              attr(tmp, "name") <- "norm.short.inge"
            }
          }
        }
        else {
          if (directed == FALSE) {
            attr(tmp, "name") <- "norm.ge"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "norm.outge"
            }
            else {
              attr(tmp, "name") <- "norm.inge"
            }
          }
        }
      }
      else {
        if (shortest.weight) {
          if (directed == FALSE) {
            attr(tmp, "name") <- "short.ge"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "short.outge"
            }
            else {
              attr(tmp, "name") <- "short.inge"
            }
          }
        }
        else {
          if (directed == FALSE) {
            attr(tmp, "name") <- "ge"
          }
          else {
            if (out) {
              attr(tmp, "name") <- "outge"
            }
            else {
              attr(tmp, "name") <- "inge"
            }
          }
        }
      }
    }

    if (!is.null(attributes(M)$ANT)) {
      test1 <- attributes(M)$ANT == "ANT data stream focal sampling single matrix"
      test2 <- attributes(M)$ANT == "ANT data stream group sampling single matrix"
      test3 <- attributes(M)$ANT == "ANT link permutations single matrix"

      test4 <- attributes(M)$ANT == "ANT data stream focal sampling multiple matrices"
      test5 <- attributes(M)$ANT == "ANT data stream group sampling multiple matrices"
      test6 <- attributes(M)$ANT == "ANT link permutations multiple matrices"
      
      # Check if argument M originates from ANTs multiples matrices importations
      test7 <- attributes(M)$ANT == "list of matrices obtained through data frames of interactions"

      if (any(test1, test2, test3)) {
        if (is.null(df)) {
          result <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
            r <- met.ge.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
            attr(r, "permutation") <- attributes(x)$permutation
            return(r)
          }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
        }
        else {
          if (!is.data.frame(df)) {
            stop("Argument df must be a data frame when argument M is an outcome of perm.ds.grp ant function", "\r")
          }
          result <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out, df, tmp) {
            df$ge <- met.ge.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
            names(df)[ncol(df)] = attributes(tmp)$name
            attr(df, "permutation") <- attributes(x)$permutation
            return(df)
          }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out, df = df,  tmp = tmp)
        }

        if (test1) {
          attr(result, "name") <- attributes(tmp)$name
          attr(result, "focal") <- attributes(M)$focal
          attr(result, "ctrl") <- attributes(M)$ctrl
          attr(result, "alters") <- attributes(M)$alters
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }

        if (test2) {
          attr(result, "name") <- attributes(tmp)$name
          attr(result, "scan") <- attributes(M)$scan
          attr(result, "ctrlf") <- attributes(M)$ctrlf
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }

        if (test3) {
          attr(result, "name") <- attributes(tmp)$name
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }
      }

      if (any(test4, test5, test6)) {
        if (is.null(df)) {
          result <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
            r1 <- lapply(x, function(y, weighted, shortest.weight, normalization, directed, out) {
              r2 <- met.ge.single(y, weighted, shortest.weight, normalization, directed, out)
              attr(r2, "permutation") <- attributes(y)$permutation
              return(r2)
            }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
            return(r1)
          }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
        }
        else {
          if (!is.null(df) & is.data.frame(df)) {
            stop("Argument df must be a list of data frames of same length as the argument df input in function perm.ds.grp.", "\r")
          }
          # Check if each matrix size is equal to the corresponding dataframe size
          # Which means we are working on a case with multiple repermutations
          # Thus with a list of lists of matrices and dataframes
          if (sum(unlist(lapply(seq_along(M), function(i, a) {
            nrow(a[[i]][[1]])
          }, a = M))) == nrow(df[[1]])) {
            tmp2 <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
              r1 <- lapply(x, function(y, weighted, shortest.weight, normalization, directed, out) {
                r2 <- met.ge.single(y, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
              }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
            }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)

            tmp2 <- do.call(Map, c(c, tmp2))

            # Paste each of the new vectors in a list of dataframes
            # and name column of dataframe according to user arguments binary, sym, out declaration
            result <- lapply(seq_along(tmp2), function(x, tmp2, df, tmp) {
              df[[x]]$ge <- tmp[[x]]
              colnames(df[[x]])[ncol(df[[x]])] <- attributes(tmp)$name
              return(df[[x]])
            }, tmp2 = tmp2, df = df, tmp= tmp)
          }
          else {
            # dataframe manipulation
            ldf <- do.call("rbind", df)

            tmp2 <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
              r1 <- lapply(x, function(y, weighted, shortest.weight, normalization, directed, out) {
                r2 <- met.ge.single(y, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
              }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
            }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)

            tmp2 <- do.call(Map, c(c, tmp2))

            result <- lapply(seq_along(tmp2), function(tmp2, tmp, ldf, i) {
              ldf$ge <- tmp[[i]]
              names(ldf)[ncol(ldf)] = attributes(tmp)$name
              attr(ldf, "permutation") <- i
              return(ldf)
            }, tmp2 = tmp2, tmp = tmp, ldf = ldf)
          }
        }

        if (test4) {
          attr(result, "name") <- attributes(tmp)$name
          attr(result, "focal") <- attributes(M)$focal
          attr(result, "ctrl") <- attributes(M)$ctrl
          attr(result, "alters") <- attributes(M)$alters
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }

        if (test5) {
          attr(result, "name") <- attributes(tmp)$name
          attr(result, "scan") <- attributes(M)$scan
          attr(result, "ctrlf") <- attributes(M)$ctrlf
          attr(result, "method") <- attributes(M)$method
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }

        if (test6) {
          attr(result, "name") <- attributes(tmp)$name
          attr(result, "ANT") <- attributes(M)$ANT
          return(result)
        }
      }
      
      # If argument M originates from ANTs multiples matrices importations
      if(test7){
        if (is.null(df)) {
          result <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
            r <- met.ge.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
          }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
          attr(result, "name") <- attributes(tmp)$name
          return(result)
        }
        
        if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
          result <- mapply(function(x, y, t) {
            y$ge <- met.ge.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
            colnames(y)[ncol(y)] <- t
            return(y)
          }, x = M, y = df, t = attributes(tmp)$name, SIMPLIFY = FALSE)
          return(result)
        }
      }
    }
    else {
      if (!test & is.list(M)) {
        if (is.null(df)) {
          result <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
            r <- met.ge.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
          }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
          attr(result, "name") <- attributes(tmp)$name
          return(result)
        }

        if (!is.null(df) & !is.data.frame(df) & is.list(df)) {
          result <- mapply(function(x, y, t) {
            y$ge <- met.ge.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
            colnames(y)[ncol(y)] <- t
            return(y)
          }, x = M, y = df, t = attributes(tmp)$name, SIMPLIFY = FALSE)
          return(result)
        }
      }
    }
  }
}
